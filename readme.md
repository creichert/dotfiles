
> If I have seen further it is only by standing on the shoulders of giants.
>
> -- Isaac Newton

# creichert's dotfiles

- [`xmonad`](#xmonad)
- [`emacs`](#emacs)
  - [`haskell`](#haskell)
  - [`gnus`](#gnus)
- [`themes`](#themes)

This repo contains my entire system configuration. The
[Dockerfile](.circleci/config/Dockerfile) is a simulation of my personal Debian
installation which I use to continually verify that my configuration works.

All dotfiles in this repo are installed using minimally invasive commands and
most will not overwrite any existing data on your system.

Use `make` to:

- visualize how the install will affect your system:

      $ make simulate

- install all dotfiles with `stow`:

      $ make dotfiles

- install a single dotfile with `stow`:

      $ make dotfiles pkg=emacs


## packages

- [bash](#bash)
- [bin](#bin)
- [docs](#docs)
- [dotlocal](#dotlocal)
- [emacs](#emacs)
- [fonts](#fonts)
- [ghc](#ghc)
- [git](#git)
- [global](#global)
- [gnupg](#gnupg)
- [postgresql](#postgresql)
- [ssh](#ssh)
- [stack](#stack)
- [systemd](#systemd)
- [x11](#xll)
- [xmonad](#xmonad)

### bash

Bash configuration files and scripts.

### bin

This package contains some miscellaneous scripts I use. It also serves as an
install dir for user-specific scripts.

### dotlocal

`dotlocal` is a git repo I use to manage "private" dotfiles. I try my best to
minimize the number of files contained in this repo in favor of using `pass` as
much as possible.

### emacs

I use emacs for 95% of my work.

- `C-x d` load a new project directory
- `C-x C-d` open an existing project

once a project is opened:
- `C-x f` load a file

#### haskell

- `C-c l` reload current module in ghci
- `C-c ;` load/reload project in ghci repl

##### haskell etags

[`stack-tag`](https://github.com/creichert/stack-tag) can compile a
single etags file for a stack project including all transitive
dependencies.

### fonts

All font configurations. This configuration mostly relies on Xresources to
handle fonts but some applications still require higher-level support.

### ghc

GHC and related Haskell configuration files.

### git

Git configuration files.

### global

GNU Global configuration for source code tags.

### gnupg

**NOTE:** The gpg-agent pinentry doesn't interoperate well w/ Emacs 25. Because
of this, some GPG Agent queries might use the GTK pinenty popup.

### postgresql

Although I use Emacs' `sql-mode` for most postgresql maintenace, I still
occassionally need `psql` directly. This package contains a decent
[`.psqlrc`](postgresql/.psqlrc)

### ssh

`ssh` config files used to configure ssh-agent, identity files, and the
`~/.ssh/config.d` directory for private drop-in configurations.

### stack

My configuration files to the
[`stack`](https://docs.haskellstack.org/en/stable/README/) build tool. I rely on
`stack extensively for most Haskell development.

### systemd

Local [`systemd`](https://www.freedesktop.org/wiki/Software/systemd/) user
services. I find it's much easier to manage services in `systemd` user services
as opposed to starting them in the background from my `.xsession`.

### x11

[X11/X.Org](https://www.x.org/wiki/) configuration files. For most windows I use
standard X11 and GTK when necessary.

### xmonad

To install or reinstall xmonad/xmobar:

      $ make xmonad

or, type `Win-q`.


**keybindings:**
- `Win-f1` open new terminal
- `Win-f2` open new emacs session
- `Win-SHIFT-f2` open new emacsclient session
- `Win-p`  open any program

`xmonad` can also be re-compiled on the fly using `stack`:

- `Win-q` recompile & reload xmonad/xmobar.
- `Win-[0..9]` switch workspaces
- `Win-SPC` switch layout

#### scratchpads

Load these scratchpads on any workspace:

- `Win-k` open floating terminal
- `Win-j` Org agenda (hit `f8` when the emacs terminal opens)
- `Win-r` My agenda

## themes

Get a list of themes:

    $ make theme theme=chalk

Install a theme:

    $ make theme q=chalk

Which will take effect when an application, or the entire X session is
restarted. Themes are generated using Xresources. Default settings can be
found in `x11/.Xresources`.


## misc. posts & hacks

- [Validating JSON Schema documents with Org mode](docs/validate-json-schema-with-org-mode.md)
- [webpack-dev-server.el - An Emacs mode to help you manage webpack-dev-server](https://creichert.io/webpack-dev-server.el)
- [Fetch unknown PGP keys in Gnus](docs/fetch-unknown-pgp-keys-in-gnus.md)
- [Useful BBDB v3 auto notes](docs/useful-bbdb-auto-notes.md)
- [self-documenting Makefiles](docs/self-documenting-makefiles.md)
- [`flycheck-haskell` in stack projects](docs/flycheck-haskell-in-stack-projects.md)
- [clean chromium system fonts](docs/clean-chromium-system-fonts.md)

---

> [_dotfiles.github.io_](https://dotfiles.github.io/)
> [dotshare](http://dotshare.it/dots/1027/)
