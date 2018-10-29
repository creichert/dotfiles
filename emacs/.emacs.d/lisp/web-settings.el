

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


(use-package flowmacs
  :load-path "site-lisp/flowmacs"
  :hook ((web-mode . flowmacs-mode))
  :bind (("C-c f f" . flowmacs/jump-to-def)
         ("C-c f t" . flowmacs/type-at-pos))
  :init
  (creichert/set-flowmacs-flow)
  :preface
  (defun creichert/set-flowmacs-flow ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory) "node_modules"))
           (flow (and root (expand-file-name "node_modules/.bin/flow" root))))
      (when (and flow (file-executable-p flow))
        ;; Set binary path
        (setq-local flowmacs/+flow+ flow)))))


(provide 'web-settings)
