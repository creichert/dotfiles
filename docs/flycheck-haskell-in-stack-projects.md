---
title: Flycheck haskell in Stack projects
author: creichert
---

When stack first started coming out there were a few diffierent flycheck
plugins looming on github. I wrote one for my own personal usage here:

```elisp
;; This is a flycheck plugin for stack I wrote before support wass added
;; to flycheck-haskell. It doesn't work quite as well as flycheck's, as
;; it's not based around ghci/runghc w/ optimized ghc flags.

(add-to-list 'load-path (substitute-in-file-name "$HOME/.emacs.d/flycheck-haskell-stack/"))
(require 'flycheck-haskell-stack)
(flycheck-select-checker 'haskell-stack)
```

Since then, the functionality has been built into `flycheck-haskell`, an
officially maintained checker for flycheck. The good thing about using
`flycheck-haskell`, as opposed to working on my own (albeit tiny) checker
is that I get access to all the crowdsourced performance improvements
from contributors. I also consider `flycheck-haskell` a win-win because
it require virtuall no configuration.


Installing the melpa package should be sufficient:
`M-x package-install RETS flycheck-haskell`

If you need further configuration, you can configure most aspects of the
`ghc` invocation:

```
(ddrequire 'flycheck-haskell)
(setq flycheck-ghc-args '("-Wall"))
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
```

In my day-to-day development work with Haskell, I use a combination of:

- flycheck-haskell
- haskell-mode ghci sessions (also display inline errors)
- ghcid

With these tools, manually recompilation in a shell becomes a task I need
to do significantly less often.
