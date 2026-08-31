
> If I have seen further it is only by standing on the shoulders of giants.
>
> -- Isaac Newton

# creichert's dotfiles

This repo contains my dotfiles and other misc scripts for system setup. All
dotfiles in this repo are installed using minimally invasive commands and most
will not overwrite any existing data on the system.

Use `make` to:

- visualize how the install will affect your system:

      ```$ make # make simulate```

- install all dotfiles with `stow`:

      ```$ make dotfiles```

- install a single dotfile with `stow`:

      ```$ make dotfiles pkg=hypr```

- update emacs:

      ```$ make elpa && make dotemacs```
        

## misc. posts & hacks (OLD)

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
