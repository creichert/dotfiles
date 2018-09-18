
# creichert dotfiles.
#
# This Makefile manages dotfiles using `stow` and also has helpers for
# bootstrapping a new system with packages I use the most.

PROGRAMS	:= emacs xmonad bash gnupg postgresql ssh x11 ghc git fonts stack
STOW_FLAGS	:= --ignore=.*.gpg --ignore=.*.pem
STACK_VERSION	:= 1.7.1
THEME           ?= $(or $(q),mocha-256)
THEME_DIR       := x11/.local/share/base16-xresources/xresources
XMONAD          := $(HOME)/.xmonad/xmonad-x86_64-linux
XMOBAR_BIN      := $(HOME)/.local/bin/xmobar
XMONAD_BIN      := $(HOME)/.local/bin/xmonad



.PHONY: dotfiles
dotfiles: $(THEME_DIR)
	stow ${STOW_FLAGS} ${PROGRAMS} --verbose --target=$(HOME)


.PHONY: clean
clean:
	stow -D ${PROGRAMS}


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
theme: $(THEME_DIR)
	@xrdb -remove
	@xrdb -merge ~/.Xresources
	@xrdb -merge $(THEME_DIR)/base16-$(THEME).Xresources
	@xrdb -override $(THEME_DIR)/base16-$(THEME).Xresources


themes-list: $(THEME_DIR)
	@echo
	@echo "Usage: $ make theme q=mocha[-256]"
	@echo
	@ls -1 $(THEME_DIR)/base16-*.Xresources		\
		| xargs basename --suffix=.Xresources	\
		| grep -v 256				\
		| sed 's/base16-//g'			\
		| xargs basename --suffix=.Xresources	\
		| column

$(THEME_DIR):
	@git submodule update --init $(shell dirname $(THEME_DIR))

dotemacs:
	@emacs --batch --debug-init -l emacs/.emacs -eval "(progn \
		  (print window-system) \
		  (print \"emacs config initialized successfully\") \
		)"

elpa:
	rm -rf $(HOME)/.emacs.d/elpa
	$(MAKE) dotemacs
