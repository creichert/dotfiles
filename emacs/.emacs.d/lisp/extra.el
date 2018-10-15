
;;; Code:

(require 'use-package)
;;(require 'use-package-ensure-system-package)


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
  :load-path "site-lisp/")


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
  :ensure t
  :disabled)


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



;; https://github.com/chrisdone/chrisdone-emacs/blob/master/packages/lpaste/lpaste.el
;;
;; needs curl
(defun lpaste-region (beg end)
  "Paste the region to lpaste.net."
  (interactive "r")
  (let ((response
         (shell-command-to-string
          (format "curl -D/dev/stdout \"http://lpaste.net/new?%s\""
                  (mapconcat 'identity
                             (mapcar (lambda (cons)
                                       (concat (url-hexify-string (car cons))
                                               "="
                                               (url-hexify-string (cdr cons))))
                                     `(("title" . ,(read-from-minibuffer "Title: "))
                                       ("author" . ,lpaste-author)
                                       ("language" . ,(cond ((eq major-mode 'haskell-mode)
                                                             "haskell")
                                                            ((eq major-mode 'emacs-lisp-mode)
                                                             "elisp")
                                                            (t
                                                             "")))
                                       ("channel" . "")
                                       ("paste" . ,(buffer-substring-no-properties beg end))
                                       ("private" . "private")
                                       ("email" . "")))
                             "&")))))
    (when (string-match "Location: /\\([0-9]+\\)" response)
      (message "%S" (match-string 1 response))
      (browse-url (concat "http://lpaste.net/"
                          (match-string 1 response))))))



(provide 'extra)

;;; extra.el ends here
