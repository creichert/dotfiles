# Quickshell Roadmap

This temporary document records current constraints and future decisions for
the Quickshell migration. Review it after each milestone and remove it before
merging the `quickshell` branch.

## Current State

- Desktop workstation: `DP-1`, 3440x1440, Ethernet, no NetworkManager.
- Laptop: `eDP-1`, 2560x1440 at scale 1.66666, NetworkManager-managed Wi-Fi.
- Quickshell runs on the desktop through UWSM; Waybar is disabled there.
- Quickshell owns `org.freedesktop.Notifications`; Mako is retired.
- `Super+P` opens the native Quickshell application launcher.
- Wofi remains only for `uuctl` service management and the `cliphist` picker.

## Principles

- Complete and polish the desktop experience before laptop integration.
- Prefer native Quickshell surfaces while keeping reliability ahead of native
  integration.
- Keep desktop and laptop differences explicit. Do not require NetworkManager
  on the Ethernet-connected desktop.
- Treat global Qt and GTK theme integration as deliberate desktop policy, not
  an application-specific workaround.
- Include required portal packages in the Arch bootstrap; keep optional RTKit
  installation separate until a feature needs it.

## Theme And UX Contract

- Use warm charcoal surfaces, cream primary text, muted secondary text, and
  terracotta urgent states. Exact palette values remain open to live testing.
- Reserve forest green for active, focused, or attention-worthy states. It is
  a signal, not general decoration.
- Use a lighter-to-deeper forest-green active-border gradient in Hyprland.
  Use solid, quieter green indicators inside Quickshell surfaces.
- Prefer thin borders and separators, minimal visual noise, and subtle 3-5px
  rounding over pronounced cards, gradients, or shadows.
- Make Quickshell surfaces keyboard-first: visible selection, `Escape` to
  dismiss or go back, `Enter` to confirm, and arrow or Vim-style movement where
  appropriate. Mouse interaction is a convenience, not the only control path.
- Keep Hyprland and Quickshell theme definitions independent for now. Defer
  shared theme generation and global Qt/GTK configuration until their value is
  clear.

## Completed Foundations

- Desktop bar, StatusNotifier tray, notifications, notification management,
  and application launcher are implemented.
- Notification state is session-only. Keep the existing action and history
  policy unless real client behavior establishes a need to change it.
- The launcher supports metadata search, configurable exclusions, session-only
  popularity ranking, keyboard navigation, focused-monitor placement, and
  optional desktop actions.
- Launcher wrappers receive `.desktop[:action]` references. UWSM is the
  default wrapper; direct Quickshell execution is supported when no wrapper is
  configured. The UWSM `kitty-open.desktop` terminal-selection issue is
  external to the launcher.

## Upcoming Work

### Theming And Icon Integration

- Choose a desktop-wide Qt icon-theme integration independently of GTK theming.
- Evaluate a Qt platform theme, such as `qt6ct`, before pinning an icon theme in
  Quickshell.
- Verify themed application, status, and fallback icons through Qt's resolver.
- Avoid local fallback assets unless a correctly configured theme remains
  unreliable.

### Wofi Retirement

- Retire `Super+Shift+P` `uuctl wofi` service management unless a concrete
  native replacement is needed.
- Keep `cliphist` as the history backend and replace the Wofi picker on
  `Super+Shift+V` with a native Quickshell surface.
- Remove Wofi only after those remaining workflows are covered or deliberately
  retired.

### Power Controls

- Replace the Waybar power menu with a native Quickshell surface.
- Require explicit confirmation for suspend, reboot, and shutdown.

### Media Controls

- Add native MPRIS display and basic playback controls.

### Laptop Integration

- Add explicit host configuration, laptop-sized density, battery, and backlight
  support.
- Build NetworkManager Wi-Fi controls for scan, known-network connection,
  WPA-PSK entry, disconnect, and forget.
- Keep enterprise Wi-Fi, VPN, captive portals, and unusual authentication as
  fallback cases until needed.
- Remove laptop `nm-applet` only after the native replacement is verified.

### Final Cleanup

- Complete a cohesive visual polish pass after functional behavior is stable.
- Remove unused Waybar, Wofi, and related configuration deliberately.

## Open Decisions

- Determine the durable Quickshell host-selection mechanism.
- Define desktop-network behavior if future features need a shared network
  surface without NetworkManager.
- Revisit tray presentation only if real StatusNotifier consumers require it.
