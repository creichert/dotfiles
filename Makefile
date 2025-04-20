
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
	hypr
	# ssh
	# ghc
	# stack

PACKAGES	:= $(or $(pkg),$(ALL_PACKAGES))

# The location you want to install packages to
PKG_DIR         ?= $(or $(target),$(HOME))


# Use --no-folding to avoid linking directories
# (e.g. .emacs.d is too high level)
STOW_FLAGS := --verbose -v1 --target=$(PKG_DIR)
STOW_FLAGS += --ignore="gnupg/.gnupg/.*.gpg"	\
		--ignore=".*.pem"		\
		--ignore=".*TAGS"		\
		--ignore="flycheck_.*"		\
		--ignore=".*.rej"		\
		--ignore=".*.swp"		\
		--ignore=".*~"			\
		--ignore=".gnus"		\
		--ignore=".*local/bin/kvm_.*"	\
		--ignore="dotlocal/"

.PHONY: simulate
simulate: #submodules
	@stow $(STOW_FLAGS) --simulate $(PACKAGES)
#	-@[ -d "./dotlocal" ] && make -C dotlocal/ simulate

.PHONY: dotfiles
dotfiles: #submodules
	@stow $(STOW_FLAGS) --target=$(PKG_DIR) $(PACKAGES)
#	-@[ -d "./dotlocal" ] && make -C dotlocal/ dotfiles

.PHONY: clean
clean:
	@stow $(STOW_FLAGS) -D $(PACKAGES)




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

# dockerfile:
# 	cd .circleci/images && \
# 	docker build -t creichert/debian \
# 		--build-arg debian_mirror_url="http://httpredir.debian.org/debian" \
# 		.
# 	docker push creichert/debian
