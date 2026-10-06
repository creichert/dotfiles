#!/usr/bin/env bash

set -euo pipefail

# One startup capability scan, not a sampler. Keep reads and brightnessctl
# adjustments on the same device; no hostname or monitor assumptions.
for backlight in /sys/class/backlight/*; do
    [[ -r "$backlight/brightness" && -r "$backlight/max_brightness" ]] || continue
    brightness=$(< "$backlight/brightness") || continue
    maximum=$(< "$backlight/max_brightness") || continue
    [[ "$brightness" =~ ^[0-9]+$ && "$maximum" =~ ^[0-9]+$ ]] || continue
    (( maximum > 0 && brightness <= maximum )) || continue

    device=${backlight##*/}
    device=${device//\\/\\\\}
    device=${device//\"/\\\"}
    device=${device//$'\n'/\\n}
    device=${device//$'\r'/\\r}
    device=${device//$'\t'/\\t}
    adjustment_supported=false
    if command -v brightnessctl > /dev/null 2>&1; then
        adjustment_supported=true
    fi
    printf '{"device":"%s","maximum":%s,"adjustmentSupported":%s}\n' \
        "$device" "$maximum" "$adjustment_supported"
    exit 0
done

printf '{"device":"","maximum":0,"adjustmentSupported":false}\n'
