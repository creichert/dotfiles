
# This Makefile manages dotfiles using `stow` and also has helpers for
# bootstrapping a new system with packages I use the most.
#
# See the README.md for more information about how symlinks are built.

PACKAGES	:= emacs xmonad bash gnupg postgresql ssh x11 ghc git fonts stack bin

# The location you want to install packages to
PKG_DIR         ?= $(or $(target),$(HOME))

THEME           ?= $(or $(q),mocha-256)
THEME_DIR       := x11/.local/share/base16-xresources/xresources

XMONAD          := $(HOME)/.xmonad/xmonad-x86_64-linux
XMOBAR_BIN      := $(HOME)/.local/bin/xmobar
XMONAD_BIN      := $(HOME)/.local/bin/xmonad


STOW_FLAGS := --verbose -v1 --target=$(PKG_DIR)
STOW_FLAGS += --ignore="gnupg/.gnupg/.*.gpg" --ignore=.*.pem

.PHONY: simulate
simulate: submodules
	@stow ${STOW_FLAGS} --simulate ${PACKAGES}

.PHONY: dotfiles
dotfiles: submodules
	@stow ${STOW_FLAGS} -v1 --target=$(PKG_DIR) ${PACKAGES}

.PHONY: dotlocal
dotlocal:
	[ -d "./dotlocal" ] && make -C dotlocal/ dotfiles


.PHONY: clean
clean:
	@stow -D -v1 ${PACKAGES}


# https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/
.PHONY: xmonad $(XMONAD)
xmonad: $(XMONAD)
$(XMONAD): xmonad/.xmonad/xmonad.hs $(XMONAD_BIN) $(XMOBAR_BIN)
	$(shell stack path --local-bin)/xmonad --recompile
	$(shell stack path --local-bin)/xmonad --restart

$(XMONAD_BIN): stack/.stack/global-project/stack.yaml stack/.stack/config.yaml
	cd $(PWD) && stack install xmonad xmonad-contrib
$(XMOBAR_BIN): stack/.stack/global-project/stack.yaml stack/.stack/config.yaml
	cd $(PWD) && stack install xmobar


# New base16 themes: https://github.com/chriskempson/base16
theme: submodules
	@fc-cache -vf
	@xrdb -remove
	@xrdb -merge ~/.Xresources
	@xrdb -merge $(THEME_DIR)/base16-$(THEME).Xresources
	@xrdb -override $(THEME_DIR)/base16-$(THEME).Xresources


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

# Check for git submodules which are not initialized (prefixed with "-").
#
# It's possible check if they not initialized _or_ dirty using '^[-]|^[+]'
.PHONY: submodules
submodules:
	@if git submodule status | egrep -q '^[-]'; then \
		git submodule update --init;                 \
	fi

dotemacs:
	@emacs --batch --debug-init \
		--eval='(setq use-package-verbose t)' \
		--eval='(load "~/.emacs")' \
		--eval='(use-package-report)' \
		--eval='(message "%s" (with-current-buffer "*use-package statistics*" (buffer-string)))' \
		--eval='(message "startup took %s" (emacs-init-time))' \
		--eval='(message "use pkg min time  %s" use-package-minimum-reported-time)'

elpa:
	rm -rf $(HOME)/.emacs.d/elpa
	$(MAKE) dotemacs

emacsdaemon:
	-emacsclient -e '(kill-emacs)'
	emacs --daemon --eval '(use-package org-protocol :demand t)'
