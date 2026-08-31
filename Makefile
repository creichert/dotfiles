
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
	wofi \
	mako \
	kitty \
	vim \
	hypr

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
#
# - hyprshot: scripted in bin/
.PHONY: arch
arch:
	sudo pacman -S base-devel \
		git \
		stow \
		vim \
		emacs-wayland \
		uwsm \
		hyprland \
		kitty \
		hyprpaper \
		hyprsunset \
		inotify-tools \
		hyprpicker \
		wofi \
		mako \
		pass \
		wl-clipboard \
		cliphist \
		playerctl \
		adw-gtk-theme \
		ttf-hack-nerd \
		noto-fonts-emoji
