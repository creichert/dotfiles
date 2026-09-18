# Quickshell Roadmap

This temporary document records the remaining desktop work and deferred
follow-ups for the Quickshell migration. Review it after each milestone. Before
merging the `quickshell` branch, transfer post-merge work to durable tracking
and remove this document.

## Current State

- Desktop workstation: `DP-1`, 3440x1440, Ethernet, no NetworkManager.
- Laptop: `eDP-1`, 2560x1440 at scale 1.66666, NetworkManager-managed Wi-Fi.
- Quickshell runs on the desktop through UWSM; Waybar is disabled there.
- Quickshell owns `org.freedesktop.Notifications`; Mako is retired.
- `Super+P` opens the native Quickshell application launcher.
- Wofi remains only for `uuctl` service management and the `cliphist` picker.
- The XDG portal broker and Hyprland/GTK backends are installed. The packaged
  Hyprland policy already routes through `hyprland;gtk`; no user override is
  needed.
- GTK uses `adw-gtk3-dark` with `color-scheme` set to `prefer-dark`. Chromium
  uses its GTK system theme, and Chromium and OrcaSlicer dialogs are verified.

## Principles

- Complete and polish the desktop experience without expanding this branch
  into full laptop feature parity.
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
  shared theme generation and global Qt configuration until their value is
  clear.

## Completed Foundations

- Desktop bar, StatusNotifier tray, notifications, notification management,
  and application launcher are implemented.
- Notification state is session-only. Keep the existing action and history
  policy unless real client behavior establishes a need to change it.
- The notification center uses a focused layer-shell panel so bar and IPC
  toggles work without an `xdg_popup` input serial. Actions run after the panel
  releases focus so clients can activate workspaces and scratchpads.
- Regular notification popups keep only the newest three visible. Exact
  duplicate content replaces the older popup, while every notification and
  its distinct actions remain available in history. Critical popups remain
  pinned until dismissed.
- The bar, notification center, and toast host are created only on the selected
  primary screen. Single-screen hosts do not require an explicit monitor name.
- The launcher supports metadata search, configurable exclusions, session-only
  popularity ranking, keyboard navigation, focused-monitor placement, and
  optional desktop actions.
- Launcher wrappers receive `.desktop[:action]` references. UWSM is the
  default wrapper; direct Quickshell execution is supported when no wrapper is
  configured. The UWSM `kitty-open.desktop` terminal-selection issue is
  external to the launcher.
- GTK integration no longer relies on a `GTK_THEME` environment override.
  GTK3 theme and portal color-scheme settings must be changed together if a
  future theme picker or scheduler is implemented.

## Remaining Desktop Work

### Theming And Icon Integration

- Treat the current GTK dark configuration as the tested baseline. Defer a
  repository theme helper and Quickshell theme picker until theme switching is
  a concrete feature.
- Choose a desktop-wide Qt icon-theme integration independently of GTK theming.
- Evaluate a Qt platform theme, such as `qt6ct`, before pinning an icon theme in
  Quickshell.
- Verify themed application, status, and fallback icons through Qt's resolver.
- Avoid local fallback assets unless a correctly configured theme remains
  unreliable.
- Defer end-to-end ScreenCast portal testing until a screen-sharing workflow is
  needed; the interface and Hyprland backend are present.

### Wofi Retirement

- Retire the `Super+Shift+P` `uuctl wofi` service-management binding without a
  replacement.
- Keep `cliphist` as the history backend and replace the Wofi picker on
  `Super+Shift+V` with a native Quickshell surface.
- Remove Wofi only after the clipboard picker is verified and no remaining
  desktop workflow uses it.

### Final Cleanup

- Run final static checks and desktop runtime validation after the clipboard
  picker and Wofi retirement are complete.
- Audit Waybar, Mako, Wofi, `uuctl`, and related package/configuration references.
  Remove only items that are no longer needed by a supported host.
- Complete a final cohesive visual polish pass.

## Post-Merge Work

### Media Controls

- Add native MPRIS display and basic playback controls in a focused follow-up.

### Laptop Integration

- Start with the smallest host-aware changes needed to run the current
  Quickshell surfaces on the laptop; avoid coupling initial compatibility to
  full laptop feature parity.
- Add explicit host configuration and laptop-sized density.
- Add battery and backlight support.
- Build NetworkManager Wi-Fi controls for scan, known-network connection,
  WPA-PSK entry, disconnect, and forget.
- Keep enterprise Wi-Fi, VPN, captive portals, and unusual authentication as
  fallback cases until needed.
- Remove laptop Waybar, Wofi, Mako, and `nm-applet` only after their Quickshell
  replacements are verified.
