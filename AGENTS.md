# AGENTS.md

## Repository

This is a personal dotfiles repository managed with GNU Stow.

Top-level directories such as `hypr/`, `quickshell`, `emacs/`, `kitty/`,
`bash/`, etc are independent Stow packages. Dotfiles are managed via `Makefile`.

## Safety

Do not automatically:

- reload or restart Hyprland
- apply or restow configuration unless instructed (simulation via `make` is okay
  for testing)
- install system packages
- modify files outside this repository unless instructed

Hyprland reload/restart is explicitly user-controlled because it can disrupt the
active desktop session.

Quickshell source changes may be auto-reloaded by a running Quickshell instance.
That is expected and acceptable. Quickshell changes may therefore affect the
live shell while being edited; avoid unnecessary disruptive changes and validate
them normally.

For other changes that affect the live desktop environment, stop after
validation and let me review them before activation.

## Validation

Use appropriate syntax/static checks where available.

- For Hyprland Lua changes, validate the Lua/config structure without activating
  or reloading Hyprland.
- emacs: see `dotemacs` rule in Makefile. Other elisp syntax checks are fine and
  compilation checks are fine.
- quickshell: see `quickshell-check` in Makefile

Explain any validation that cannot be performed safely without affecting the
live environment.
