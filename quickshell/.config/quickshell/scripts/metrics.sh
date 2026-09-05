#!/usr/bin/env bash

set -euo pipefail

previous_total=0
previous_idle=0
sample_number=0
temperature_c=0

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

    if (( sample_number % 3 == 1 )); then
        for temperature_file in /sys/bus/pci/drivers/k10temp/0000:00:18.3/hwmon/hwmon*/temp1_input; do
            if [[ -r "$temperature_file" ]]; then
                temperature_millidegrees=$(< "$temperature_file")
                temperature_c=$((temperature_millidegrees / 1000))
                break
            fi
        done
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
    printf '{"cpuPercent":%s,"memoryPercent":%s,"temperatureC":%s,"interfaceName":"%s","receiveBytes":%s,"transmitBytes":%s,"timestamp":%s}\n' \
        "$cpu_percent" "$memory_percent" "$temperature_c" "$interface_name" "$receive_bytes" "$transmit_bytes" "$timestamp_ms"
    sleep 2
done
