#!/usr/bin/env bash

set -euo pipefail

temperature_hwmon_path=${1:?missing temperature hwmon path}
interval_seconds=${2:?missing metrics interval}
temperature_interval_samples=${3:?missing temperature interval samples}

previous_total=0
previous_idle=0
sample_number=0
temperature_c=0
battery_capacity_file=""
battery_status_file=""
brightness_file=""
brightness_max_file=""

discover_power_devices() {
    for power_supply in /sys/class/power_supply/*; do
        [[ -d "$power_supply" ]] || continue
        [[ -r "$power_supply/type" ]] || continue
        [[ "$(<"$power_supply/type")" == "Battery" ]] || continue

        if [[ -r "$power_supply/capacity" && -r "$power_supply/status" ]]; then
            battery_capacity_file="$power_supply/capacity"
            battery_status_file="$power_supply/status"
            break
        fi
    done

    for backlight in /sys/class/backlight/*; do
        [[ -r "$backlight/brightness" && -r "$backlight/max_brightness" ]] || continue
        brightness_file="$backlight/brightness"
        brightness_max_file="$backlight/max_brightness"
        break
    done
}

# Device names under /sys/class are sufficient for the lifetime of this
# sampler. Discovering them once avoids filesystem scans on every sample.
discover_power_devices

while true; do
    read -r _ user nice system idle iowait irq softirq steal _ < /proc/stat
    total=$((user + nice + system + idle + iowait + irq + softirq + steal))

    if (( sample_number == 0 )); then
        cpu_percent=0
    else
        total_delta=$((total - previous_total))
        idle_delta=$((idle - previous_idle))
        cpu_percent=$((100 * (total_delta - idle_delta) / total_delta))
    fi

    previous_total=$total
    previous_idle=$idle
    sample_number=$((sample_number + 1))

    memory_total=0
    memory_available=0
    while read -r key value _; do
        case "$key" in
            MemTotal:) memory_total=$value ;;
            MemAvailable:) memory_available=$value ;;
        esac
    done < /proc/meminfo
    memory_percent=$((100 * (memory_total - memory_available) / memory_total))

    if (( sample_number % temperature_interval_samples == 1 )); then
        for temperature_file in "$temperature_hwmon_path"/hwmon*/temp1_input; do
            if [[ -r "$temperature_file" ]]; then
                temperature_millidegrees=$(< "$temperature_file")
                temperature_c=$((temperature_millidegrees / 1000))
                break
            fi
        done
    fi

    battery_percent_json=null
    battery_status_json=null
    if [[ -n "$battery_capacity_file" ]]; then
        battery_percent=$(< "$battery_capacity_file")
        if [[ "$battery_percent" =~ ^[0-9]+$ ]] && (( battery_percent <= 100 )); then
            battery_percent_json=$battery_percent
        fi

        battery_status=$(< "$battery_status_file")
        case "$battery_status" in
            Charging|Discharging|Full|Unknown)
                battery_status_json="\"$battery_status\""
                ;;
            "Not charging")
                battery_status_json='"Not charging"'
                ;;
        esac
    fi

    brightness_percent_json=null
    if [[ -n "$brightness_file" ]]; then
        brightness=$(< "$brightness_file")
        brightness_max=$(< "$brightness_max_file")
        if [[ "$brightness" =~ ^[0-9]+$ && "$brightness_max" =~ ^[0-9]+$ ]] \
            && (( brightness_max > 0 )); then
            brightness_percent_json=$((100 * brightness / brightness_max))
        fi
    fi

    interface_name=""
    while read -r interface destination _ _ _ _ _ _; do
        if [[ "$destination" == "00000000" ]]; then
            interface_name="$interface"
            break
        fi
    done < <(tail -n +2 /proc/net/route)

    receive_bytes=0
    transmit_bytes=0
    if [[ -n "$interface_name" ]]; then
        read -r _ < /proc/net/dev
        read -r _ < /proc/net/dev
        while read -r name receive _ _ _ _ _ _ _ transmit _; do
            if [[ "$name" == "$interface_name:" ]]; then
                receive_bytes=$receive
                transmit_bytes=$transmit
                break
            fi
        done < /proc/net/dev
    fi

    timestamp_ms=${EPOCHREALTIME/./}
    timestamp_ms=${timestamp_ms:0:13}
    printf '{"cpuPercent":%s,"memoryPercent":%s,"temperatureC":%s,"batteryPercent":%s,"batteryStatus":%s,"brightnessPercent":%s,"interfaceName":"%s","receiveBytes":%s,"transmitBytes":%s,"timestamp":%s}\n' \
        "$cpu_percent" "$memory_percent" "$temperature_c" "$battery_percent_json" "$battery_status_json" "$brightness_percent_json" "$interface_name" "$receive_bytes" "$transmit_bytes" "$timestamp_ms"
    sleep "$interval_seconds"
done
