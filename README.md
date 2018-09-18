# creichert's dotfiles

- [**`xmonad`**](#xmonad)
- [**`emacs`**](#emacs)
  - [`haskell`](#haskell)
- [**`themes`**](#themes)

All dotfiles in this repo are installed in a minimally invasive way and
will not overwrite any existing data on your system

**install**

visualize how the install will affect your system.

    $ make simulate

install all dotfiles with `stow`

    $ make dotfiles

install a single dotfile with `stow`

    $ make dotfiles pkg=emacs

install/reinstall xmonad config

    $ make xmonad

## xmonad

- **open new terminal** `M-f1`
- **open new emacs session** `M-f2`
- **open any program** `M-p`

My xmonad configuration is built using `stack`.

- `M-q` will **recompile & reload** xmonad & xmobar.
- `M-[0..9]` switch workspaces
- `M-SPC` switch layout

### scratchpads

Load these scratchpads on any workspace:

- `M-K` **terminal**
- `M-J` **terminal 2**

## emacs

- `C-x C-d` **open project**

### haskell

- **load/reload project in ghci repl** `C-l`
- **reload current module in ghci** `C-l`
- **jump to definition** `f`

## themes

Get a list of themes:

    $ make theme q=chalk

Install a theme:

    $ make theme q=chalk

Which takes effect when an application, or the entire X session is
restarted.

> [_dotfiles.github.io_](https://dotfiles.github.io/)
