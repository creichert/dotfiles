# Quickshell Roadmap

This is a temporary working roadmap for migrating desktop-shell features to
Quickshell. Review it after each completed milestone and remove it before
merging the `quickshell` branch.

## Current State

- Primary development target: desktop workstation.
- Desktop display: `DP-1`, 3440x1440, Ethernet-connected.
- Desktop does not use NetworkManager or `nm-applet`.
- Laptop display: `eDP-1`, 2560x1440, scale 1.66666.
- Laptop uses NetworkManager and `nm-applet` for Wi-Fi configuration.
- Laptop-specific Waybar differences currently exist as local/stashed changes:
  - Backlight module.
  - Battery module.
- Quickshell currently targets `DP-1` and is UWSM-managed by Hyprland.
- Waybar is disabled in Hyprland startup.
- Mako and Wofi remain active and unchanged.
- Waybar, Mako, Wofi, and their packages remain installed until final cleanup.
- Branch rollback is the recovery path until merge: switch to `master` and
  restart the Hyprland session.

## Principles

- Complete and polish the desktop workstation experience before laptop work.
- Prefer native Quickshell services and surfaces over separate legacy tools.
- Reliability takes priority over native integration when the two conflict.
- Do not run Waybar and Quickshell as the normal concurrent desktop bars.
- Replace Mako atomically because only one daemon can own
  `org.freedesktop.Notifications`.
- Keep host-specific behavior explicit and minimal in committed configuration.
- Do not make NetworkManager a requirement for the Ethernet-connected desktop.
- Defer visual redesign until functional behavior is stable.
- GPU monitoring remains optional and outside the MVP.
- Portal and RTKit installation are external system work, not a dotfiles change.

## Milestones

### 1. Completed: Bar Foundation

- Quickshell bar MVP is implemented and manually verified.
- Bar configuration and components were refactored.
- The window title is centered on the full panel.
- The bar currently appears only on `DP-1`.
- Commits:
  - `d8306eb Add initial Quickshell bar`
  - `a6b0816 Refactor Quickshell bar configuration`
  - `9541ee3 Center Quickshell window title`

### 2. Completed: Desktop Workstation Integration

- Scope was limited to the desktop workstation.
- Quickshell is part of the default Stow package set and is launched through
  UWSM using the stowed `~/.config/quickshell/shell.qml`.
- Waybar is disabled in the same Hyprland activation path.
- A minimal auto-hiding StatusNotifier tray host occupies no bar space when
  empty and expands when an application registers an item.
- Window icons use the active Hyprland toplevel's Wayland app ID.
- Validated in both an existing and a fresh Hyprland session, including newly
  created Kitty, Emacs, Chromium, primary-workspace, and scratchpad windows.
- Wofi and Mako remain unchanged.
- Commits:
  - `4e92b8e Start Quickshell with UWSM`
  - `2d36cf7 Start desktop services before applications`
  - `f9a8a79 Use Wayland app IDs for window icons`

### 3. Notification System

- Design before implementation.
- Scope:
  - Notification toasts.
  - Do Not Disturb.
  - Persistent history.
  - Clear and dismiss actions.
  - Notification-center UI.
  - Notification actions.
- Replace Mako only when Quickshell is ready to exclusively own
  `org.freedesktop.Notifications`.
- Replace `makoctl` keybindings in the same activation change.
- Implement and validate on the desktop workstation first.

### 4. New Quickshell-Native UX

- Design native Quickshell surfaces instead of copying basic Waybar
  tooltips/popups.
- Scope:
  - Application launcher.
  - Power controls.
  - Media display and controls.
  - Richer interactions where they provide real value.
- Decide whether clipboard selection moves with the launcher or remains
  Wofi-based temporarily.
- Keep GPU monitoring out of scope unless a clear use case appears.

### 5. Laptop Integration

- Port the proven desktop implementation with minimal host-specific changes.
- Add explicit Quickshell host configuration equivalent to the existing
  `HOSTNAME == "laptop"` Hyprland distinction.
- Target `eDP-1` and adapt sizing/density for the smaller effective display.
- Add battery and backlight modules.
- Build native Quickshell networking on NetworkManager rather than retaining
  `nm-applet`.
- Laptop networking MVP:
  - Wi-Fi enable/disable.
  - Scan and list visible networks.
  - Signal strength and security state.
  - Connect to known networks.
  - WPA-PSK password entry for new networks.
  - Disconnect and forget actions.
- Prevent laptop-only `nm-applet` autostart after the native replacement is
  verified.
- StatusNotifier tray support remains available for unrelated applications.
- Do not assume desktop Ethernet metrics or controls generalize to the laptop.
- Enterprise Wi-Fi, VPN configuration, captive portals, and unusual
  authentication remain fallback cases until needed.

### 6. Visual Redesign

- Choose a cohesive visual direction after functional behavior is stable on
  both hosts.
- Preserve the current restrained bar appearance as the baseline until then.
- Revisit accents, state styling, and module-specific presentation only as
  part of this deliberate redesign.

### 7. Final Cleanup

- After both hosts are stable and the branch is ready to merge, remove unused
  Waybar, Mako, Wofi, and related configuration/packages deliberately.
- Remove this roadmap document before merge.
- Do not mix cleanup with feature activation milestones.

## Open Decisions

- Determine whether `HOSTNAME == "laptop"` is the durable host-selection
  mechanism for Quickshell.
- Define desktop-network behavior if future features need a unified network
  surface without NetworkManager.
- Revisit tray presentation only if real StatusNotifier consumers create a
  use case.
- Install and validate planned portal and RTKit packages separately.
