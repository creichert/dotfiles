
;;; Code:

(require 'use-package)


(use-package ggtags
  :defer
  :requires (evil)
  :ensure t
  :commands (ggtags-mode
             ggtags-find-reference
             ggtags-idutils-query
             pop-tag-mark)
  ;;:ensure-system-package
  ;;((gtags    . "global")
  ;; (pip      . "pip install pygments"))
  ;; (pygments . "pip install pygments"))
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  :bind (
         ("M-," . pop-tag-mark)
         ("M-/" . ggtags-find-reference)
         ("M-]" . ggtags-idutils-query)
         :map ggtags-navigation-map
         ("M-u" . ggtags-navigation-previous-file)
         ("M-o" . ggtags-navigation-next-file)
         ("M-l" . ggtags-navigation-visible-mode)
         ("M-j" . ggtags-navigation-visible-mode)
         ("M-k" . next-error)
         ("M-i" . previous-error)))


(use-package auth-source-pass
  :ensure t
  :defer
  :config
  (auth-source-pass-enable))


(use-package dired-rainbox
  :disabled
  :ensure t)


(use-package etags-select
  :disabled
  :load-path "site-lisp/etags-select.el/")


(use-package gist
  :defer
  ;;:ensure-system-package (git)
  :ensure t)


(use-package ledger-mode
  :disabled
  ;;:ensure-system-package (ledger)
  :ensure t
  :defer)


(use-package flycheck-ledger
  :disabled
  :ensure t
  :after (flycheck ledger-mode)
  :hook  ((ledger-mode . flycheck-ledger-mode)))


(use-package gif-screencast
  :defer
  :ensure t
  :if window-system
  ;;:ensure-system-package (gifsicle
  ;;                        mogrify
  ;;                        imagemagick)
  :init
  (setq gif-screencast-screenshot-directory "~/downloads/screencasts/tmp")
  (setq gif-screencast-output-directory "~/downloads/screencasts")
  :bind
  (([f11] . gif-screencast)
   ([f12] . gif-screencast-stop)))


(use-package term
  ;;:commands (make-term term ssh-term)
  ;;:ensure-system-package (ssh)
  :init
  (defun remote-term (new-buffer-name cmd &rest switches)
    (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
    (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
    (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
    (set-buffer term-ansi-buffer-name)
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer term-ansi-buffer-name))

  (defun ssh-term (host)
    (interactive "sHost: \n")
    (remote-term (format "ssh-%s" host) "ssh" (format "%s" host))))


(use-package magit-gh-pulls
  :ensure t
  :hook (magit-status-mode . magit-gh-pulls-mode)
  :init
  (setq magit-gh-pulls-pull-detail-limit 30)
  (setq magit-gh-pulls-status-documentation t))


(use-package github-clone
  :defer
  :ensure t)



(use-package alert
  :defer
  :ensure t)


(use-package org-gcal
  :after (auth-source-pass)
  :ensure t
  :defer
  :commands (org-gcal-fetch org-gcal-sync)
  ;; :custom
  ;; (org-gcal-auto-archive t)
  :config
  (setq org-gcal-client-id (auth-source-pass-get "user" "developers.google.com/org-gcal")
        org-gcal-file-alist '(("creichert07@gmail.com" . "~/org/cal.org"))
        org-gcal-client-secret (auth-source-pass-get 'secret "developers.google.com/org-gcal")))




(use-package noflet
  :defer
  :ensure t)


(use-package kill-ring-ido
  :requires (ido noflet)
  :custom (kill-ring-ido-shortage-length 40) ; where 6 is your value
  :bind (("C-c k" . kill-ring-ido))
  :load-path "site-lisp/")


(use-package pdf-tools
  :disabled
  ;;:load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))



(use-package org-gcal
  :requires (alert auth-source-pass)
  :ensure t
  :commands (org-gcal-fetch org-gcal-sync)
  :defer
  :config
  (setq org-gcal-client-id (auth-source-pass-get "user" "developers.google.com/org-gcal")
        org-gcal-file-alist '(("creichert07@gmail.com" . "~/org/cal.org"))
        org-gcal-client-secret (auth-source-pass-get 'secret "developers.google.com/org-gcal")))



(use-package yasnippet
  :defer
  :ensure t
  :after (evil-leader)
  :hook ((haskell-mode . yas-minor-mode))
  :config
  (use-package yasnippet-snippets
    :disabled
    :ensure t)
  (use-package haskell-snippets
    :ensure t)
  ;;(yas-global-mode t)
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
  (evil-leader/set-key-for-mode 'magit-status-mode
    "SPC" 'yas-expand-maybe)
  :diminish yas-minor-mode)


(use-package lpaste
  :load-path "lisp"
  :if (file-exists-p "~/.emacs.d/lisp/lpaste.el"))


(use-package slack-settings
  :load-path "lisp"
  :if (file-exists-p "~/.emacs.d/lisp/slack-settings.el"))


(use-package erc-settings
  :load-path "lisp"
  :if (file-exists-p "~/.emacs.d/lisp/erc-settings.el"))


(provide 'extra)

;;; extra.el ends here
