

(use-package haskell-mode
  :ensure t
  :after (compile evil)
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'"   . haskell-core-mode)
         ("\\.lhs\\'"   . literate-haskell-mode))
  :interpreter
  ("env stack"  . haskell-mode)
  ("stack"      . haskell-mode)
  ("runhaskell" . haskell-mode)
  ("haskell"    . haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c h t" . haskell-process-do-type)
              ("C-c h h" . hoogle)
              ("C-c h T" . haskell-session-change-target)
              ("C-c h i" . haskell-process-do-info)
              ("C-c C-;" . haskell-process-load-file)
              ("C-c C-l" . haskell-process-reload)
              ("C-c i"   . haskell-navigate-imports-go)
              ("C-c I"   . haskell-navigate-imports-return)
              )
  :custom
  (haskell-stylish-on-save t)
  ;; enable debugging
  (haskell-process-log t)
  (haskell-process-suggest-haskell-docs-imports t)
  (haskell-process-suggest-restart nil)

  :config
  (setq haskell-indentation-electric-flag t)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-starter-offset 4)

  ;; print type info to presentation-mode instead
  ;; of message area.
  (setq haskell-process-use-presentation-mode t)
  (setq haskell-process-auto-import-loaded-modules t)

  ;; bytecode takes up more memory than object code.
  ;; enable
  ;; (haskell-process-reload-with-fbytecode t)

  (add-to-list 'haskell-font-lock-quasi-quote-modes '("yamlQQ" . yaml-mode))
  (add-to-list 'haskell-font-lock-quasi-quote-modes '("js"     . web-mode))
  ; "--ghci-options=-fshow-loaded-modules"
  (add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-O0")

  (evil-set-initial-state 'haskell-presentation-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  (evil-leader/set-key-for-mode 'haskell-mode
    "hir" 'hindent-reformat-region
    "hid" 'hindent-reformat-decl-or-fill
    "f"   'haskell-mode-jump-to-def-or-tag
    "TAB" 'haskell-hide-toggle
    "l"   'haskell-process-load-or-reload)

  (add-to-list 'electric-layout-rules
               '((?\{ . around) (?\} . around)))
  ;(add-to-list 'electric-layout-rules
  ;             '((?\{) (?\} . around)))

  (add-to-list 'compilation-error-regexp-alist
        'haskell-compilation-error-regexp-alist)

  :hook
  ((haskell-mode . haskell-doc-mode))
  ((haskell-mode . haskell-collapse-mode))
  ((haskell-mode . haskell-decl-scan-mode))
  ((haskell-mode . haskell-indentation-mode))
  ((haskell-mode . electric-layout-mode))
  ((haskell-mode . electric-pair-local-mode))
  ((haskell-mode . electric-indent-local-mode))
  ((haskell-mode . prettify-symbols-mode))
  ((haskell-interactive-mode . next-error-follow-minor-mode))
  )


(use-package hindent
  :ensure t
  :after (haskell-mode)
  :hook ((haskell-mode . hindent-mode)))


(use-package flycheck-haskell
  :ensure t
  :after (flycheck haskell-mode)
  :hook ((haskell-mode . flycheck-haskell-setup))
  :custom
  ;(flycheck-ghc-args '("-Wall" "-O0" "-fno-code"))
  (flycheck-ghc-args '("-O0"))
  :init
  (setq flycheck-ghc-search-path '("."))
  ;; monorepo
  ;;
  ;; maybe check if:
  ;; - current dir has stack.yaml
  ;; - does current directory have a package.yaml
  ;; - subdirs have package.yaml
  ;;
  ;; load extensions appropriately
  (add-to-list 'flycheck-ghc-language-extensions "DerivingStrategies")
  (add-to-list 'flycheck-ghc-language-extensions "RecordWildCards")
  (add-to-list 'flycheck-ghc-language-extensions "DeriveGeneric")
  (add-to-list 'flycheck-ghc-language-extensions "DeriveLift")
  (add-to-list 'flycheck-ghc-language-extensions "LambdaCase")
  (add-to-list 'flycheck-ghc-language-extensions "ViewPatterns")
  (add-to-list 'flycheck-ghc-language-extensions "QuasiQuotes")
  (add-to-list 'flycheck-ghc-language-extensions "TemplateHaskell")
  (add-to-list 'flycheck-ghc-language-extensions "CPP")
  (add-to-list 'flycheck-ghc-language-extensions "StandaloneDeriving")
  (add-to-list 'flycheck-ghc-language-extensions "NoImplicitPrelude")
  )


;; automatically apply hlint suggestions when applicable
(use-package hlint-refactor
  ;;:ensure-system-package ((refactor . "stack install apply-refact"))
  :after (haskell-mode)
  :ensure t
  :hook ((haskell-mode . hlint-refactor-mode))
  :bind (("C-c h r" . hlint-refactor-refactor-at-point)
         ("C-c h R" . hlint-refactor-refactor-buffer)))


;; (use-package ghcid
;;   ;;:ensure-system-package ((ghcid . "stack install ghcid"))
;;   :defer
;;   :load-path "site-lisp/"
;;   :bind (:map projectile-mode-map
;;               ("C-c m s" . ghcid)
;;               ("C-c m b" . show-ghcid-buf)
;;               ("C-c m t" . set-ghcid-target))
;;   :custom
;;   (ghcid-target "")
;;   ;;:config (setq-local default-directory projectile-project-root)
;;   :preface
;;   (use-package haskell-mode :ensure t)
;;   (defun show-ghcid-buf ()
;;     (interactive)
;;     (show-buffer ghcid-buf-name))
;;   (defun set-ghcid-target (ghcid-targ &optional ghcid-test-targ)
;;     (interactive
;;      (list
;;       (completing-read "ghcid target: " (map 'list (lambda (targ) (format "%s:%s" (projectile-project-name) targ)) (haskell-cabal-enum-targets)))
;;       (completing-read "ghcid --test target: " '("--test=main" "--test=Main.main" nil))))
;;     (setq ghcid-target ghcid-targ)
;;     (when ghcid-test-targ
;;       (setq ghcid-target-test (format "%s" ghcid-test-targ)))
;;     (kill-ghcid)
;;     (ghcid)))


(provide 'haskell-settings)

;;; haskell-settings.el ends here
