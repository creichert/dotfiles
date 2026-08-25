

;; add node_modules to `exec-path`
(use-package add-node-modules-path
  :ensure t
  :defer)


(use-package prettier-js
  :ensure t
  :defer)


(use-package web-mode
  :defer
  :ensure t
  :mode "\\.js\\'"
  :hook
  ((web-mode . web-mode-init))
  ((web-mode . electric-pair-local-mode))
  ((web-mode . electric-indent-local-mode))
  ((web-mode . electric-layout-mode))
  :preface
  (defun web-mode-init ()
    (add-node-modules-path)
    (prettier-js-mode))
  :init (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
              ;; might have to set in web-mode-hook
              web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4)

  :config
  (use-package flycheck :ensure t)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)

  (add-to-list 'electric-layout-rules
               '((?\{ . around) (?\} . around)))
  (add-to-list 'electric-layout-rules
               '((?\[ . around) (?\] . around))))



(provide 'web-settings)
