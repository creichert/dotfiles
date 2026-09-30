#!/usr/bin/env bash

set -euo pipefail

temperature_hwmon_path=${1:?missing temperature hwmon path}
interval_seconds=${2:?missing metrics interval}
temperature_interval_samples=${3:?missing temperature interval samples}

previous_total=0
previous_idle=0
sample_number=0
temperature_c=null
temperature_sensor_name=""
temperature_sensor_label=""
temperature_supported=false
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

# Sensor identity is stable for this sampler lifetime. Keep dynamic hwmon
# numbering out of the configuration, and avoid extra metadata reads per sample.
for temperature_file in "$temperature_hwmon_path"/hwmon*/temp1_input; do
    [[ -r "$temperature_file" ]] || continue
    temperature_supported=true
    temperature_directory=${temperature_file%/*}
    if [[ -r "$temperature_directory/name" ]]; then
        temperature_sensor_name=$(< "$temperature_directory/name")
    fi
    if [[ -r "$temperature_directory/temp1_label" ]]; then
        temperature_sensor_label=$(< "$temperature_directory/temp1_label")
    fi
    break
done

# Escape strings without adding a JSON helper process to the shared sampler.
json_string() {
    local target=$1 value=$2
    value=${value//\\/\\\\}
    value=${value//\"/\\\"}
    value=${value//$'\n'/\\n}
    value=${value//$'\r'/\\r}
    value=${value//$'\t'/\\t}
    printf -v "$target" '"%s"' "$value"
}
json_string temperature_name_json "$temperature_sensor_name"
json_string temperature_label_json "$temperature_sensor_label"
printf -v temperature_identity_json '"temperatureSensorName":%s,"temperatureSensorLabel":%s' \
    "$temperature_name_json" "$temperature_label_json"

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

    if (( (sample_number - 1) % temperature_interval_samples == 0 )); then
        temperature_c=null
        for temperature_file in "$temperature_hwmon_path"/hwmon*/temp1_input; do
            if [[ -r "$temperature_file" ]]; then
                temperature_supported=true
                temperature_millidegrees=$(< "$temperature_file") || temperature_millidegrees=""
                if [[ "$temperature_millidegrees" =~ ^-?[0-9]+$ ]]; then
                    temperature_c=$((temperature_millidegrees / 1000))
                fi
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
    gateway_hex=""
    while read -r interface destination gateway _ _ _ _ _; do
        if [[ "$destination" == "00000000" ]]; then
            interface_name="$interface"
            gateway_hex="$gateway"
            break
        fi
    done < <(tail -n +2 /proc/net/route)

    gateway_json=null
    if [[ "$gateway_hex" =~ ^[[:xdigit:]]{8}$ && "$gateway_hex" != "00000000" ]]; then
        printf -v gateway_ip '%d.%d.%d.%d' \
            "$((16#${gateway_hex:6:2}))" "$((16#${gateway_hex:4:2}))" \
            "$((16#${gateway_hex:2:2}))" "$((16#${gateway_hex:0:2}))"
        gateway_json="\"$gateway_ip\""
    fi

    link_state="unknown"
    if [[ -n "$interface_name" && -r "/sys/class/net/$interface_name/operstate" ]]; then
        link_state=$(< "/sys/class/net/$interface_name/operstate")
    fi

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
    cpu_sample_valid=false
    (( sample_number > 1 )) && cpu_sample_valid=true
    printf '{"cpuPercent":%s,"cpuSampleValid":%s,"memoryPercent":%s,"memoryTotalBytes":%s,"memoryAvailableBytes":%s,"temperatureC":%s,"temperatureSupported":%s,%s,"batteryPercent":%s,"batteryStatus":%s,"brightnessPercent":%s,"interfaceName":"%s","gateway":%s,"linkState":"%s","receiveBytes":%s,"transmitBytes":%s,"timestamp":%s}\n' \
        "$cpu_percent" "$cpu_sample_valid" "$memory_percent" "$((memory_total * 1024))" "$((memory_available * 1024))" "$temperature_c" "$temperature_supported" "$temperature_identity_json" "$battery_percent_json" "$battery_status_json" "$brightness_percent_json" "$interface_name" "$gateway_json" "$link_state" "$receive_bytes" "$transmit_bytes" "$timestamp_ms"
    sleep "$interval_seconds"
done
