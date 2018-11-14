
# This Makefile manages dotfiles using `stow` and also has helpers for
# bootstrapping a new system with packages I use the most.
#
# See the README.md for more information about how symlinks are built.

ALL_PACKAGES	:= \
	emacs \
	xmonad \
	bash \
	gnupg \
	postgresql \
	ssh \
	x11 \
	ghc \
	git \
	fonts \
	stack \
	bin \
	global \
	systemd

PACKAGES	:= $(or $(pkg),$(ALL_PACKAGES))

# The location you want to install packages to
PKG_DIR         ?= $(or $(target),$(HOME))

THEME           ?= $(or $(theme),mocha-256)
THEME_DIR       := docs/assets/base16-xresources/xresources

XMONAD          := $(HOME)/.xmonad/xmonad-x86_64-linux
XMOBAR_BIN      := $(HOME)/.local/bin/xmobar
XMONAD_BIN      := $(HOME)/.local/bin/xmonad


STOW_FLAGS := --verbose -v1 --target=$(PKG_DIR)
STOW_FLAGS += --ignore="gnupg/.gnupg/.*.gpg"	\
		--ignore=".*.pem"		\
		--ignore=".*TAGS"		\
		--ignore="flycheck_.*"		\
		--ignore=".*.rej"		\
		--ignore=".*.swp"		\
		--ignore=".*~"			\
		--ignore="dotlocal/"

.PHONY: simulate
simulate: submodules
	@stow $(STOW_FLAGS) --simulate $(PACKAGES)
	-@[ -d "./dotlocal" ] && make -C dotlocal/ simulate

.PHONY: dotfiles
dotfiles: submodules
	@stow $(STOW_FLAGS) -v1 --target=$(PKG_DIR) $(PACKAGES)
	-@[ -d "./dotlocal" ] && make -C dotlocal/ dotfiles

.PHONY: clean
clean:
	@stow $(STOW_FLAGS) -D $(PACKAGES)


# https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/
.PHONY: xmonad $(XMONAD)
xmonad: $(XMONAD)
$(XMONAD): xmonad/.xmonad/xmonad.hs $(XMONAD_BIN) $(XMOBAR_BIN)
	$(shell stack path --local-bin)/xmonad --recompile
	$(shell stack path --local-bin)/xmonad --restart

$(XMONAD_BIN): stack/.stack/global-project/stack.yaml stack/.stack/config.yaml
	cd $(PWD) && stack install xmonad xmonad-contrib
# flags for xmobar are configured in ~/.stack/global-project/stack.yaml
$(XMOBAR_BIN): stack/.stack/global-project/stack.yaml stack/.stack/config.yaml
	cd $(PWD) && stack install xmobar


# New base16 themes: https://github.com/chriskempson/base16
theme: submodules

	@# The .Xresources.d/theme script is sourced in the `.Xresources` file.
	@cat $(THEME_DIR)/base16-$(THEME).Xresources > x11/.Xresources.d/theme

	@# clears all xresources
	@xrdb -remove

	@# The default
	@# `startx` using `.xsession` will use my Xresources file as long as it\'s allowed
	@# in `/etc/X11/xsession.options`. See `man xsession` & `man xsession.options` for
	@# more info.
	@#
	@# Technically, the override shouldn\'t be needed but it\'s possible that in the
	@# future some system "default" xsession scripts initialize resources that conflict
	@# with my colors. So, override.
	@xrdb -merge -override ~/.Xresources

	@-xrdb -query | grep -v '^$$'

	@# TODO
	@#dunst_xr_theme_changer.sh
	@#mv ~/.config/dunst/dunstrc_xr_colors x11/.config/dunst/dunstrc

	-xscreensaver-command -restart

themes-list: submodules
	@echo
	@echo "Usage: $ make theme q=mocha[-256]"
	@echo
	@ls -1 $(THEME_DIR)/base16-*.Xresources		\
		| xargs basename --suffix=.Xresources	\
		| grep -v 256				\
		| sed 's/base16-//g'			\
		| xargs basename --suffix=.Xresources	\
		| column


# New base16 themes: https://github.com/chriskempson/base16
.PHONY: fonts
fonts: submodules
	@rm -rf ~/.cache/fontconfig
	@fc-cache -vf
	@$(MAKE) theme


# Check for git submodules which are not initialized (prefixed with "-").
#
# It's possible check if they not initialized _or_ dirty using '^[-]|^[+]'
.PHONY: submodules
submodules:
	@if git submodule status | egrep -q '^[-]'; then \
		git submodule update --init;                 \
	fi

## Emacs recipes

dotemacs:
	@emacs --batch --debug-init										\
		--eval='(setq use-package-verbose t)'								\
		--eval='(load "~/.emacs")'									\
		--eval='(use-package-report)'									\
		--eval='(message "%s" (with-current-buffer "*use-package statistics*" (buffer-string)))'

elpa:
	rm -rf $(HOME)/.emacs.d/elpa
	$(MAKE) dotemacs

emacsdaemon:
	-emacsclient -e '(kill-emacs)'
	emacs --daemon

xflux: bin/bin/xflux
bin/bin/xflux:
	curl -L https://justgetflux.com/linux/xflux64.tgz -o xflux64.tgz
	tar xvzf xflux64.tgz
	mv xflux bin/bin/xflux
	rm xflux64.tgz
