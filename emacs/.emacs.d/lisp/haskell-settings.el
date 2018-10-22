

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" "\\.lhs\\'")
  :interpreter
  ("stack"      . haskell-mode)
  ("runhaskell" . haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c h t" . haskell-process-do-type)
              ("C-c h h" . hoogle)
              ("C-c h t" . haskell-session-change-target)
              ("C-c h i" . haskell-process-do-info)
              ("C-c C-;" . haskell-process-load-file)
              ("C-c C-l" . haskell-process-reload)
              ("C-c i"   . haskell-navigate-imports-go)
              ("C-c I"   . haskell-navigate-imports-return)
              ;;("C-c C-j" . haskell-run-function-under-cursor)
              )
  ;; TODO keep tags up-to-date even if its the damn *.tags file.
  ;; they incrementally update the xref tags tables
  :custom
  ;; enable debugging
  (haskell-process-log t)
  (haskell-process-suggest-haskell-docs-imports t)
  (haskell-process-suggest-restart nil)
  ;; indentation
  (haskell-indentation-electric-flag t)
  (haskell-indentation-layout-offset 4)
  ;;(haskell-indentation-starter-offset 4)
  (haskell-indentation-left-offset 4)
  (haskell-stylish-on-save t)
  ;;(haskell-interactive-mode-eval-mode t)
  ;;
  ;; this is set automatically when there is a `stack.yaml`
  ;; haskell-process-type 'stack-ghci
  ;; print type info to presentation-mode instead
  ;; of message area.
  ;; haskell-process-use-presentation-mode t
  ;;
  ;; bytecode takes up more memory than object code.
  ;; enable
  ;;(haskell-process-reload-with-fbytecode nil)

  ;;
  ;; experimenting with brittany
  ;; haskell-mode-stylish-haskell-path "brittany"
  :hook
  ((haskell-mode . haskell-doc-mode))
  ((haskell-mode . haskell-collapse-mode))
  ((haskell-mode . haskell-decl-scan-mode))
  ((haskell-mode . haskell-indentation-mode))
  ((haskell-mode . electric-pair-local-mode))
  ((haskell-mode . electric-indent-local-mode))
  ((haskell-mode . electric-layout-mode))
  ((haskell-mode . prettify-symbols-mode))
  :preface
  ;; https://gist.github.com/989ad8be92f68682abff
  (defun haskell-run-function-under-cursor ()
    "Send the word-at-point as a function to GHCi process."
    (interactive)
    ;; (haskell-process-set-sent-stdin 't)
    (haskell-process-send-string
     (haskell-session-process (haskell-session-maybe))
     (format "%s" (word-at-point))))

  :config
  (add-to-list 'haskell-font-lock-quasi-quote-modes '("yamlQQ" . yaml-mode))
  (add-to-list 'haskell-font-lock-quasi-quote-modes '("js"     . web-mode))
  (add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-O0")

  (evil-leader/set-key-for-mode 'haskell-mode
    "hir" 'hindent-reformat-region
    "hid" 'hindent-reformat-decl-or-fill
    "f"   'haskell-mode-jump-to-def-or-tag
    "TAB" 'haskell-hide-toggle
    "l"   'haskell-process-load-or-reload)

  ;;(add-to-list 'electric-layout-rules
  ;;             '((?\{ . around) (?\} . around)))
  (add-to-list 'electric-layout-rules
               '((?\{) (?\} . around)))
  (add-to-list 'electric-layout-rules
               '((?\[) (?\] . around))))


(use-package hindent
  :ensure t
  ;; :init (hindent-mode)
  :after (haskell-mode)
  :hook ((haskell-mode . hindent-mode)))


(use-package flycheck-haskell
  :ensure t
  :after (flycheck haskell-mode)
  :custom (flycheck-ghc-args '("-Wall"))
  :hook ((haskell-mode . flycheck-haskell-setup)))


(use-package hlint-refactor
  :ensure t
  :hook ((haskell-mode . hlint-refactor-mode))
  :bind (("C-c h r" . hlint-refactor-refactor-at-point)
         ("C-c h R" . hlint-refactor-refactor-buffer)))


(use-package ghcid
  :defer
  :load-path "site-lisp/"
  ;;:custom (ghcid-target "exe")
  :preface
  (defun show-ghcid-buf () (interactive) (show-buffer ghcid-buf-name))
  (defun set-ghcid-target (arg)
    (interactive
     (list
      (completing-read "ghcid target: " '("assertible:exe:assertible" "assertible:test:unit" nil))))
    (setq ghcid-target arg))
  :bind (:map haskell-mode-map
              ("C-c m s" . ghcid)
              ("C-c m b" . show-ghcid-buf)
              ("C-c m t" . set-ghcid-target)))


(provide 'haskell-settings)

;;; extra.el ends here
