

;; add node_modules to `exec-path`
(use-package add-node-modules-path
  :ensure t
  :defer)


(use-package prettier-js
  :ensure t
  :defer)


(use-package web-mode
  :ensure t
  :mode "\\.js\\'"
  :preface
  (defun web-mode-init ()
    ;;(interactive)
    (add-node-modules-path)
    (prettier-js-mode))
  :hook ((web-mode . web-mode-init))
  :init (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
              ;; might have to set in web-mode-hook
              web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4)

  ;;:hook ((web-mode . emmet-mode))
  :config
  (use-package flycheck :ensure t)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist))


(provide 'web-settings)
