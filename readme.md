
> If I have seen further it is only by standing on the shoulders of giants.
>
> -- Isaac Newton

# creichert's dotfiles

- [`xmonad`](#xmonad)
- [`emacs`](#emacs)
  - [`haskell`](#haskell)
  - [`gnus`](#gnus)
- [`themes`](#themes)

All dotfiles in this repo are installed using in a minimally invasive way and
will not overwrite any existing data on your system. I work between a desktop
and laptop and prefer to have as few differences between the two systems.

Use `make` to:

- visualize how the install will affect your system:

      $ make simulate

- install all dotfiles with `stow`:

      $ make dotfiles

- install a single dotfile with `stow`:

      $ make dotfiles pkg=emacs

- install/reinstall xmonad config:

      $ make xmonad

## xmonad

- `M-f1` open new terminal
- `M-f2` open new emacs session
- `M-SHIFT-f2` open new emacsclient session
- `M-p`  open any program

`xmonad` can be re-compiled on the fly using `stack`:

- `M-q` recompile & reload xmonad/xmobar.
- `M-[0..9]` switch workspaces
- `M-SPC` switch layout

### scratchpads

Load these scratchpads on any workspace:

- `M-K` open floating terminal
- `M-J` toggle floting terminal 2

## emacs

- `C-x d` load a new project directory
- `C-x C-d` open an existing project

once a project is opened:
- `C-x f` load a file

### haskell

- `C-l` load/reload project in ghci repl
- `C-l` reload current module in ghci
- `f` jump to definition

#### haskell etags

[`stack-tag`](https://github.com/creichert/stack-tag) can compile a
single etags file for a stack project including all transitive
dependencies.


## themes

Get a list of themes:

    $ make theme q=chalk

Install a theme:

    $ make theme q=chalk

Which will take effect when an application, or the entire X session is
restarted. Themes are generated using Xresources. Default settings can be
found in `x11/.Xresources`.


## misc. posts & hacks

- [self-documenting Makefiles](docs/self-documenting-makefiles.md)
- [`flycheck-haskell` in stack projects](docs/flycheck-haskell-in-stack-projects.md)
- [clean chromium system fonts](docs/clean-chromium-system-fonts.md)

---

> [_dotfiles.github.io_](https://dotfiles.github.io/)
> [dotshare](http://dotshare.it/dots/1027/)
