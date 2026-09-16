
# This Makefile manages dotfiles using `stow` and also has helpers for
# bootstrapping a new system with packages I use the most.
#
# See the README.md for more information about how symlinks are built.

ALL_PACKAGES := \
	emacs \
	bash \
	gnupg \
	postgresql \
	git \
	bin \
	waybar \
	quickshell \
	wofi \
	kitty \
	vim \
	hypr

QMLLINT ?= /usr/lib/qt6/bin/qmllint
QUICKSHELL_QML := $(shell git ls-files -- 'quickshell/**/*.qml')

PACKAGES	:= $(or $(pkg),$(ALL_PACKAGES))

# The location you want to install packages to
PKG_DIR         ?= $(or $(target),$(HOME))


# Do not link directories: applications can create local files without writing
# into the dotfiles repository through a folded directory symlink.
STOW_FLAGS := --no-folding --verbose -v1 --target=$(PKG_DIR)
STOW_FLAGS += --ignore=".*local/bin/kvm_.*"	\
		--ignore=".*.rej"		\
		--ignore=".*.swp"		\
		--ignore=".*screenrc.*"

.PHONY: simulate
simulate: submodules
	@stow $(STOW_FLAGS) --simulate $(PACKAGES)

.PHONY: dotfiles
dotfiles: submodules
	@stow $(STOW_FLAGS) --target=$(PKG_DIR) $(PACKAGES)

.PHONY: clean
clean:
	@stow $(STOW_FLAGS) -D $(PACKAGES)

.PHONY: quickshell-check
quickshell-check:
	@test -x "$(QMLLINT)" || { echo "qmllint not found: $(QMLLINT)"; exit 1; }
	@test -n "$(QUICKSHELL_QML)" || { echo "no Quickshell QML files found"; exit 1; }
	@$(QMLLINT) -I /usr/lib/qt6/qml $(QUICKSHELL_QML)
	@qs --private-check-compat



# Check for git submodules which are not initialized (prefixed with "-").
#
# It's possible check if they not initialized _or_ dirty using '^[-]|^[+]'
.PHONY: submodules
submodules:
	@if git submodule status | grep -E -q '^[-]'; then \
		git submodule update --init;                 \
	fi

## Emacs recipes

dotemacs:
	@emacs --batch --debug-init										\
		--eval='(setq use-package-verbose t)'								\
		--eval='(setq use-package-compute-statistics t)'						\
		--eval='(package-initialize)'									\
		--eval='(load "~/.emacs")'									\
		--eval='(use-package-report)'									\
		--eval='(message "%s" (with-current-buffer "*use-package statistics*" (buffer-string)))'

elpa:
	rm -rf $(HOME)/.emacs.d/elpa
	@# not strictly necessary
	@#emacs --batch --eval='(package-refresh-contents)'
	$(MAKE) dotemacs


## Arch setup
#
# base install: https://gist.github.com/mjkstra/96ce7a5689d753e7a6bdd92cdc169bae
#
# `pacman -Qe`

ARCH_CORE_PACKAGES := \
	base-devel \
	git \
	stow \
	vim \
	pass

ARCH_DESKTOP_PACKAGES := \
	uwsm \
	uuctl \
	hyprland \
	kitty \
	quickshell \
	wofi \
	emacs-wayland

ARCH_HYPRLAND_PACKAGES := \
	hypridle \
	hyprpaper \
	hyprsunset \
	hyprpicker \
	inotify-tools \
	cliphist \
	slurp \
	grim \
	playerctl \
	wl-clipboard

ARCH_PORTAL_PACKAGES := \
	xdg-desktop-portal \
	xdg-desktop-portal-hyprland \
	xdg-desktop-portal-gtk

ARCH_THEME_PACKAGES := \
	adw-gtk-theme \
	ttf-hack-nerd \
	noto-fonts-emoji

# hyprshot is a repository-provided script in bin/; grim and slurp are its
# screenshot dependencies above.
.PHONY: arch
arch:
	sudo pacman -S \
		$(ARCH_CORE_PACKAGES) \
		$(ARCH_DESKTOP_PACKAGES) \
		$(ARCH_HYPRLAND_PACKAGES) \
		$(ARCH_PORTAL_PACKAGES) \
		$(ARCH_THEME_PACKAGES)
