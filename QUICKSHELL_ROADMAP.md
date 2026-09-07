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
- Waybar and Wofi remain installed until final cleanup. Mako remains installed
  until the Milestone 3A live cutover.
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

## DPMS Freeze Diagnostic

- On 2026-09-07, one desktop freeze occurred after `hypridle` powered displays
  off and input woke them. A controlled retry with a 20-second timeout did not
  reproduce it; retain the normal five-minute timeout unless it recurs.
- The observed DPMS transition included AMDGPU display-controller `REG_WAIT`
  timeouts and Aquamarine output reconfiguration. Causality is unconfirmed.
- If it recurs, switch to a TTY and run `capture-hyprland-freeze.sh` from the
  repository root before terminating Hyprland.
- The script writes `~/.local/state/hyprland-freeze-<timestamp>.log` with
  Hyprland thread states and GDB backtraces plus recent kernel and user
  journals.
- The prior core dump was captured after forced shutdown began, so it cannot
  diagnose the live freeze. Capture before recovery is required.

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
  - `1fa05e7 Guard special workspace state`

### 3. Notification System

#### 3A. Completed: Toast Replacement

- Replace Mako with a Quickshell `NotificationServer` as the exclusive owner
  of `org.freedesktop.Notifications`.
- Mako was uninstalled before a fresh Hyprland session. Quickshell now owns
  `org.freedesktop.Notifications`; its `makoctl` bindings were replaced with
  notification-controller IPC bindings.
- Scope is limited to `DP-1` desktop toasts:
  - Top-right host with at most three visible regular notifications.
  - Bounded FIFO overflow queue without application grouping.
  - Controller-owned five-second normal timeout that ignores application
    expiry requests.
  - Critical notifications remain visible until dismissed.
  - Session-only notification records survive Quickshell reloads but not a
    Hyprland session restart.
  - `Super+Escape` dismisses visible toasts.
  - `Super+Ctrl+Escape` re-shows the most recently dismissed toast.
  - Notification action buttons are supported; a toast click invokes an
    explicit `default` action when present.
  - Notification bodies support styled markup and user-clicked `http`/`https`
    links. Strip body image tags before rendering.
- Do not advertise or implement body images, action icons, inline replies, or
  non-web URL schemes in this phase.
- Use the current dark bar palette for normal notifications and
  `urgentBackgroundColor` for critical notifications.
- Validate with `notify-send`, Chromium/Slack notifications, and OpenCode
  notification actions in an existing and fresh Hyprland session.
- Initial `notify-send` validation passed: normal and critical notifications,
  FIFO promotion, markup and links, controller bindings, and action buttons.
  Application icon and full client validation continue through normal use.

#### 3B. Notification Management

- Add Do Not Disturb: retain normal notifications for management while
  suppressing their toasts; critical notifications bypass Do Not Disturb.
- Add bar unread and Do Not Disturb state.
- Add a notification-center popup with session history, read state, individual
  dismissal, and clear actions.
- Place a bell/count control before the clock. Its left click opens an anchored
  popup; Do Not Disturb remains a popup-header control rather than a second bar
  button.
- Keep all notifications in the center from arrival, including currently
  visible and queued toasts. New records are unread and require explicit
  individual read/unread toggling.
- When Do Not Disturb is enabled, leave existing normal toasts alone; retain
  subsequent normal notifications in history only and never replay them on
  disable. Critical notifications continue to toast.
- Clear All removes history and dismisses every live or queued notification.
  Individual removal dismisses a live notification when it still exists.
- Use a `PopupWindow` with outside-click and Escape dismissal. It must not
  become a Hyprland-decorated focused window.
- Show 24-hour timestamps as time today, date plus time earlier this year, and
  date plus year and time in prior years.
- Preserve action-capable notifications only for the current Quickshell
  process lifetime; actions are not guaranteed after a reload.
- Keep card-click behavior without a `default` action and critical-toast
  click behavior unchanged until real application use establishes a need.

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
  Waybar, Wofi, and related configuration/packages deliberately.
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
