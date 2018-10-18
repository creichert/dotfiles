
;;; Code:

(require 'use-package)


(use-package bbdb-pgp
  :disabled
  ;;:bind (("C-c b p" . bbdb-pgp))
  ;; not initialized, use bbdb-pgp
  ;;:custom (bbdb-initialize 'pgp)
  :after (bbdb message))


;; use 'gnus for incoming messages too
;;
;; https://www.emacswiki.org/emacs/UpgradeBBDB
(use-package bbdb-anniv
  :after (bbdb)
  :config
  (bbdb-initialize 'anniv)
  (add-to-list 'bbdb-anniv-alist
               '((work . "%n's %d%s work anniversary"))))


(use-package org-bbdb
  ;; TODO work anniversary
  :disabled
  ;;:bind (("C-c o b" . org-bbdb-open))
  :after (org)
  :config
  (add-to-list 'org-bbdb-anniversary-format-alist
             '(("work" .
               (lambda (name years suffix)
                 (concat "Work Anniversary: [[bbdb:" name "][" name " ("
                         ;; handles numbers as well as strings
                         (format "%s" years)
                         suffix ")]]"))))))


(use-package bbdb-vcard
  :ensure t
  :defer
  ;;:requires (bbdb message)
  ;;:command bbdb-vcard-import-file
  ;;:custom
  ;;(bbdb-vcard-directory "~/.emacs.d/contacts"))
  )


(use-package ggtags
  :disabled
  ;; get it from package.el
  :defer
  :requires (evil)
  :ensure t
  ;; end :commands
  :commands (ggtags-mode
             ggtags-find-reference
             ggtags-idutils-query
             pop-tag-mark)
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;;(require 'subr-x)
  ;; (setq ggtags-oversize-limit 104857600)
  ;; (setq ggtags-sort-by-nearness t)
  ;;(setq ggtags-use-idutils t)
  ;;(setq ggtags-use-project-gtagsconf nil)

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
         ("M-i" . previous-error)
         ;;:map evil-motion-state-map
         ;;("f" . ggtags-find-definitions)
         ))


(use-package auth-source-pass
  :disabled
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
  :ensure t
  :defer
  :disabled
  ;;:ensure-system-package (ledger)
  )


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


(use-package erc
  :requires (auth-source-pass)
  :commands (erc)
  :defer
  :hook
  ((erc-mode . erc-spelling-mode)
   ;; (erc-mode . erc-track-mode)
   (erc-mode . (lambda ()
                 (set (make-local-variable 'scroll-conservatively) 100))))

  :custom
  (erc-user-full-name "creichert")
  (erc-nick-uniquifier "_")
  (erc-autojoin-timing 'ident)
  (erc-server-auto-reconnect t)
  (erc-prompt-for-nickserv-password nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))

  :preface
  (defun erc-creichert ()
    (interactive)
    (erc-tls :server "irc.freenode.net" :port 6697 :nick "creichert"))

  :config
  (erc-track-minor-mode 1)
  (erc-track-mode 1)

  :init
  (setq
   erc-log-channels-directory "~/.emacs.d/erc/logs/"
   ;; erc-log-write-after-insert t

   erc-auto-discard-away t
   erc-input-line-position -2

   ;;Kill buffers for channels after /part
   ;;erc-kill-buffer-on-part t

   ;;Kill buffers for private queries after quitting the server
   ;;erc-kill-queries-on-quit t

   ;;Kill buffers for server messages after quitting the server
   ;;erc-kill-server-buffer-on-quit t

   ;; utf-8 always and forever
   erc-server-coding-system '(utf-8 . utf-8)

   ;;TEST: Interpret mIRC-style color commands in IRC chats
   erc-interpret-mirc-color t

   ;;If someone sends a /notice don't just show it in the server buffer,
   ;;but also in the mini-buffer.
   erc-echo-notices-in-minibuffer-flag t
   erc-button-url-regexp
   "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"
   erc-autojoin-channels-alist
   '((".*\\.freenode.net"
      "#hlug"
      "#haskell"
      "#emacs"
      "#debian"
      "#gnus"
      "#xmonad"
      "#yesod"
      ))))


(use-package flowmacs
  :disabled
  :load-path "site-lisp/flowmacs")


(use-package noflet
  :disabled
  :ensure t)


(use-package kill-ring-ido
  :disabled
  :requires (ido noflet)
  :custom (kill-ring-ido-shortage-length 40) ; where 6 is your value
  :bind (("M-y" . kill-ring-ido))
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



(use-package slack
  :disabled
  :ensure t
  :defer
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "assertible"
   :default t
   :client-id "creichert07@gmail.com"
   :client-secret (auth-source-pass-get 'secret "slack/assertible/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/assertible/creichert")
   :subscribed-channels '(dev general notifications ops gh ci)
   :full-and-display-names t))


;; extra emacs packages & utilities I use which aren't "core"
(use-package lpaste
  :load-path "lisp"
  :if (file-exists-p "~/.emacs.d/lisp/lpaste.el"))

(provide 'extra)

;;; extra.el ends here
