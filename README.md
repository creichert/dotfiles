
My computing environment: `xmonad`, `emacs`, & `haskell`

- **open new terminal** `M-f1`
- **open new emacs session** `M-f2`
- **open any program** `M-p`

Use the `Makefile` to:
- `make` Install dotfiles using `stow`
- `make themes-list`
- `make theme q=chalk` Change the theme

If you want to visualize what running `make` will do to your system, use
`make --dry-run --trace`

which can bootstrap the dotfiles using `stow`
## XMonad

My xmonad configuration is built using `stack`.

- `M-q` will **recompile & reload** xmonad & xmobar.
- `M-[0..9]` switch workspaces
- `M-SPC` switch layout

### Scratchpads

Load these scratchpads on any workspace:

- `M-K` **terminal**
- `M-J` **terminal 2**

## emacs

- `C-x C-d` **open project**

### Haskell

- **load/reload project in ghci repl** `C-l`
- **reload current module in ghci** `C-l`
- **jump to definition** `f`

## Setting a theme

# Load theme and merge w/ .Xresources

    $ make theme q=chalk


> [_dotfiles.github.io_](https://dotfiles.github.io/)
