

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'"   . haskell-core-mode)
         ("\\.lhs\\'"   . literate-haskell-mode))
  :interpreter
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
              ("C-c h j" . haskell-run-function-under-cursor)
              ("C-c C-j" . haskell-run-last-function)
              )
  ;; TODO keep tags up-to-date even if its the damn *.tags file.
  ;; they incrementally update the xref tags tables
  :custom
  (my-haskell-current-function "main")
  ;; enable debugging
  (haskell-process-log t)
  (haskell-process-suggest-haskell-docs-imports t)
  (haskell-process-suggest-restart nil)
  ;; indentation
  (haskell-indentation-electric-flag t)
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-left-offset 4)
  (haskell-indentation-starter-offset 4)
  ;; breaks often & doesn't work w/ even the most basic customizations
  ;;
  ;;(haskell-interactive-mode-eval-mode t)
  (haskell-stylish-on-save t)
  (haskell-tags-on-save t)
  ;;
  ;; this is set automatically when there is a `stack.yaml`
  ;; haskell-process-type 'stack-ghci
  ;; print type info to presentation-mode instead
  ;; of message area.
  (haskell-process-use-presentation-mode t)
  (haskell-process-auto-import-loaded-modules t)
  ;; bytecode takes up more memory than object code.
  ;; enable
  ;; (haskell-process-reload-with-fbytecode t)
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
  ((haskell-interactive-mode . next-error-follow-minor-mode))
  :preface
  ;; see `haskell-process-insert-type` for expanding on this solution
  (defun haskell-run-function-under-cursor ()
    (interactive)
    (let ((ident (haskell-ident-at-point)))
      (when ident
        (setq my-haskell-current-function ident)
        (let ((process (haskell-interactive-process))
              (query ident))
          (haskell-process-queue-command
           process
           (make-haskell-command
            :state (list process query (current-buffer))
            :go (lambda (state)
                  (haskell-process-send-string (nth 0 state)
                                               (nth 1 state)))
            :complete (lambda (state response)
                        (cond
                         ;; TODO: Generalize this into a function.
                         ((or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                          (message "%s" response))
                         (t (haskell-command-echo-or-present response))))))))))
  (defun haskell-run-last-function ()
    ;; TODO completing-read the comint haskell-process repl history
    ;; and use that instead
    (interactive)
    (let ((ident my-haskell-current-function))
      (when ident
        (setq my-haskell-current-function ident)
        (let ((process (haskell-interactive-process))
              (query ident)
              )
          (haskell-process-queue-command
           process
           (make-haskell-command
            :state (list process query (current-buffer))
            :go (lambda (state)
                  (haskell-process-send-string (nth 0 state)
                                               (nth 1 state)))
            :complete (lambda (state response)
                        (cond
                         ;; TODO: Generalize this into a function.
                         ((or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                          (message "%s" response))
                         (t (haskell-command-echo-or-present response))))))))))
  :config
  (add-to-list 'haskell-font-lock-quasi-quote-modes '("yamlQQ" . yaml-mode))
  (add-to-list 'haskell-font-lock-quasi-quote-modes '("js"     . web-mode))
  (add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-O0")
  ;;(add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-fshow-loaded-modules")

  (use-package evil :ensure t :demand)
  (evil-set-initial-state 'haskell-presentation-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
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


;; automatically apply hlint suggestions when applicable
(use-package hlint-refactor
  ;;:ensure-system-package ((refactor . "stack install ghc-exactprint"))
  :after (haskell-mode)
  :ensure t
  :hook ((haskell-mode . hlint-refactor-mode))
  :bind (("C-c h r" . hlint-refactor-refactor-at-point)
         ("C-c h R" . hlint-refactor-refactor-buffer)))


(use-package ghcid
  ;;:ensure-system-package ((ghcid . "stack install ghcid"))
  :defer
  :load-path "site-lisp/"
  :bind (:map projectile-mode-map
              ("C-c m s" . ghcid)
              ("C-c m b" . show-ghcid-buf)
              ("C-c m t" . set-ghcid-target))
  :custom
  (ghcid-target "")
  ;;:config (setq-local default-directory projectile-project-root)
  :preface
  (defun show-ghcid-buf ()
    (interactive)
    (show-buffer ghcid-buf-name))
  (defun set-ghcid-target (ghcid-targ &optional ghcid-test-targ)
    (interactive
     (list
      (completing-read "ghcid target: " (map 'list (lambda (targ) (format "%s:%s" (projectile-project-name) targ)) (haskell-cabal-enum-targets)))
      (completing-read "ghcid --test target: " '("--test=main" "test/unit/Data/OpenApiSpec.hs" nil))))
    (setq ghcid-target ghcid-targ)
    (when ghcid-test-targ
      (setq ghcid-target-test (format "%s" ghcid-test-targ)))
    (kill-ghcid)
    (ghcid)))


(provide 'haskell-settings)

;;; haskell-settings.el ends here
