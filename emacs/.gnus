;;; package --- My gnus email core setup
;;
;;
;;; Commentary:
;;
;;   - To setup a gmail account, you have to either to setup 2-step verification
;;     and create an "app" password (a 16 character password for a single app)
;;     or enable the setting "Allow less secure apps: ON" (sometimes required on
;;     gsuite apps.
;;
;;   - For each nnimap account, there needs to be an entry in the
;;     ~/.authinfo.gpg.  The gmail app password is used here (or the user
;;     password of "less secure apps" are enabled
;;
;;   - Outgoing mail is sent w/ `msmtp`.  Multiple outgoing smtp accounts with
;;     the same server are configured in `~/.msmtp'.
;;
;;          defaults
;;          tls on
;;          auto_from on
;;          logfile ~/.msmtp.log
;;          host smtp.gmail.com
;;          tls on
;;          tls_certcheck off
;;          auth on
;;          port 587
;;
;;          account home
;;          from myemail@gmail.com
;;          user myemail@gmail.com
;;          password PASSWORD
;;
;;          account work
;;          from me@gsuite.com
;;          user me@gsuite.com
;;          password INSERT_REALLY_LONG_PASSWORD_FROM_PSAS
;;
;;   - News and mail are handled a little differently due to reply conventions.
;;     Posting a reply on a newsgroup is called follow up, whereas sending a
;;     reply to an email message is called reply-to.
;;
;; Keybindings:
;;
;;   - g   :
;;   - ^   : view servers
;;   - r   : reply
;;   - R   : reply & cite
;;   - SW  : "site-wide" reply & quote (all cc's)

;;; Code:

(require 'use-package)


;; my gnus-private file contains various email accounts info.
;;
;; - gnus-parameters
;; - gnus-topic-topology
;; - gnus-select-method
;;
;; only load if this file exists
(use-package gnus-private
  :demand
  :if (file-exists-p "~/.emacs.d/gnus/gnus-private.el")
  :load-path "gnus/")


(use-package gnus
  :after (gnus-private)
  :preface
  (defun reload-dotgnus ()
    "Reload init file without restarting Emacs."
    (interactive)
    (load-file "~/.gnus"))
  :hook
  ((gnus-select-group       . gnus-group-set-timestamp))
  ((gnus-after-exiting-gnus . kill-emacs))
  :bind (("C-\\" . smex)
         ("C-c ;" . reload-dotgnus)
         :map gnus-group-mode-map
              ("j" . gnus-group-next-group)
              ("k" . gnus-group-prev-group))
  :config
  (setq gnus-check-new-newsgroups nil)
  (setq gnus-interactive-exit nil
        gnus-completing-read 'gnus-ido-completing-read
        gnus-asynchronous t

        ;;gnus-message-replysign t
        ;;gnus-treat-x-pgp-sig t

        ;;gnus-list-groups-with-ticked-articles nil
        gnus-group-list-inactive-groups nil ;; list-groups-with-ticked-articles nil

        gnus-default-charset 'utf-8
        gnus-default-posting-charset 'utf-8

        gnus-large-newsgroup 100
        gnus-use-cache t
        gnus-button-url 'browse-url-browser-function

        gnus-mime-view-all-parts t
        gnus-mime-display-multipart-related-as-mixed t

        gnus-gcc-mark-as-read t

        ;; only needed for compatibility w/ other mail readers
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil

        ;; Group line
        gnus-group-line-format "%M\ %S\ %p\ %P\ %5y:%B%(%g%) %P(%L)\n"

        ;; Summary line
        ;;
        ;; alternative summary lines:
        ;;
        ;;   - ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n"
        ;;
        ;;   - "%U%R%z%B%(%[%4L: %-23,23f%]%) %s")
        ;;
        gnus-summary-line-format " %R%U%z %4k | %(%~(pad-right 16)&user-date; | %-25,25f %ub | %B%s%)\n"

        ;; gnus-user-date-format-alist
        ;; '((t . "%Y-%m-%d %H:%M"))

        ;; improve gmail support
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)

        ;;message-highlight-citation t
        ;;gnus-suppress-duplicates t
        gnus-message-highlight-citation t
        gnus-article-highlight t

        gnus-treat-highlight-citation t
        gnus-treat-highlight-signature t
        gnus-treat-buttonize t
        gnus-treat-fill-long-lines nil
        ;;gnus-treat-fill-article nil


        ;;gnus-list-groups-with-ticked-articles nil

        ;; REQUIRED HERE (trouble w/ mm-decode use-package def) to render html
        ;; properly
        mm-inline-text-html-with-images t
        mm-text-html-renderer 'gnus-w3m

        ;;gnus-cloud-synced-files
        ;;'("~/.authinfo.gpg"
        ;;  "~/.gnus"
        ;;  "~/.emacs.d/bbdb"
        ;;  ;;"~/.emacs.d/gnus/*"
        ;;  ;;"~/.emacs.d/gnus/*"
        ;;  ;;(:directory "~/Mail" :match ".*")
        ;;  ;;(:directory "~/emacs.d/gnus" :match ".*")
        ;;  (:directory "~/org" :match ".*.org\\'")
        ;;  (:directory "~/org" :match ".*.org_archive\\'")
        ;;  ))
        )
  :init

  ;; (add-hook 'gnus-group-mode-hook 'gnus-agent-mode)
  ;; Gnus/Evil keybindings (only use basics in some modes)
  (evil-add-hjkl-bindings gnus-browse-mode-map  'emacs)
  (evil-add-hjkl-bindings gnus-server-mode-map  'emacs)
  (evil-add-hjkl-bindings gnus-article-mode-map 'emacs)
  (evil-add-hjkl-bindings gnus-group-mode-map   'emacs)
  (evil-add-hjkl-bindings gnus-summary-mode-map 'emacs "D"
    'gnus-summary-delete-article)

  ;;(gnus-add-configuration
  ;; '(article
  ;;   (horizontal 1.0
  ;;               (vertical 33 (group 1.0))
  ;;               (vertical 1.0
  ;;                         (summary 0.16 point)
  ;;                         (article 1.0)))))
  ;;
  ;; (gnus-add-configuration
  ;;  '(summary
  ;;    (horizontal 1.0
  ;;                (vertical 33 (group 1.0))
  ;;                (vertical 1.0 (summary 1.0 point)))))
  ;;
  (gnus-add-configuration '(article
                            (vertical 1.0
                                      (summary .25 point)
                                      (article 1.0))))

  ;;:custom-face
  ;;(gnus-group-mail-1 ((t (:foreground (x-get-resource "color2" "")))))
  )

(use-package bbdb-gnus
  :after (gnus)
  ;;:bind (:map gnus-article-mode
  ;;            ("C-c b d" . bbdb-display-records))
  )


(use-package gnus-gravatar
  :disabled
  :after (gnus)
  :hook
  ((gnus-article-prepare . gnus-treat-from-gravatar)
   (gnus-article-prepare . gnus-treat-mail-gravatar)))


(use-package gnus-sum
  :bind (:map gnus-summary-mode-map
              ("j" . gnus-summary-next-article)
              ("k" . gnus-summary-previous-article))
  :config
  (add-to-list
   'gnus-newsgroup-variables
   '(gnus-buttonized-mime-types
     . '("multipart/encrypted"
         "multipart/signed"
         "multipart/alternative"))))


(use-package gnus-art
  :config
  (setq gnus-visible-headers
        (concat "^Sender:\\|"
                "^X-GitHub-.*:\\|"
                "^X-Google-Sender-Delegation\\|"
                gnus-visible-headers)))

(use-package gnus-msg
  :custom
  (gnus-message-highlight-citation t))

(use-package gnus-srvr
  :bind (:map gnus-browse-mode-map
              ( "q" .  'gnus-browse-exit )))


(use-package gnus-topic
  :hook ((gnus-group-mode . gnus-topic-mode))
  :after (gnus-group)
  :bind (:map gnus-topic-mode-map
              ("?\t" . gnus-topic-select-group))
  :init
  ;; NOTE when first starting gnus, these options will effectively "kill" or
  ;; make all new groups zombies. for my purposes, this is intentional. I alway
  ;; go and whitelist new groups, and setup topics manually first or after the
  ;; fact.
  ;;
  ;; Setting up gnus topics in elisp is virtually impossible to do and maintain
  ;; topic parameters. If you know how to do this, email me.
  ;; (setq gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics)
  ;; (setq gnus-subscribe-newsgroup-method         'gnus-subscribe-topics)
  (setq gnus-topic-display-empty-topics         nil))


;; Outgoing messages sent via msmtp (config in ~/.msmptrc)
;;
;; TODO send via built-ins
;;
;;(setq message-send-mail-function 'smtpmail-send-it
;;      send-mail-function 'smtpmail-send-it
;;      smtpmail-debug-info t
;;      smtpmail-debug-verb t
;;      smtpmail-auth-credentials '(password-store)
;;      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "email@example.com" nil))
;;      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
;;
;; Use header "X-Smtp-Message-Mode"
;;
;; or
;;
;; Message settings
;;
;; Available SMTP accounts.
;; (defvar smtp-accounts
;;   '((plain    "me@foo.com" "smtp.foo.com" 25  "user-foo" "pass-foo")
;;     (login    "me@foo.com" "smtp.moo.com" 25  "user-moo" nil)
;;     (cram-md5 "me@foo.com" "smtp.hoo.com" 25  "user-hoo" nil)
;;     (ssl      "me@bar.com" "smtp.bar.com" 587 "user-bar" "pass-bar" "key" "cert")
;;     (ssl      "me@baz.com" "smtp.baz.com" 587 "user-baz" "pass-baz" "key" "cert")))
;;
(use-package message
  :preface
  ;; (message-send-hook)
  (defvar my-message-attachment-regexp
    "attach\\|\Wfiles?\W\\|enclose\\|\Wdraft\\|\Wversion")
  (defun check-mail ()
    "ask for confirmation before sending a mail. Scan for possible attachment"
    (save-excursion
      (message-goto-body)
      (let ((warning ""))
        (when (and (search-forward-regexp my-message-attachment-regexp nil t nil)
                   (not (search-forward "<#part" nil t nil)))
          (setq warning "No attachment.\n"))
        (goto-char (point-min))
        (unless (message-y-or-n-p (concat warning "Send the message ? ") nil nil)
          (error "Message not sent")))))

  (defun confirm-empty-subject ()
    "Allow user to quit when current message subject is empty."
    (or (message-field-value "Subject")
        (message-yes-or-no-p "Really send without Subject? ")
        (keyboard-quit)))
  :hook
  ;; (auto-revert-tail-mode)
  ((message-mode . flyspell-mode))
  ((message-send . check-mail))
  ;; ensure subject is not empty
  ((message-send . confirm-empty-subject))
  ((message-send . ispell-message))

  :bind (:map message-mode-map
              ( "\t"  . bbdb-complete-mail ))
  :config
  ;; References From Date)
  (add-to-list 'message-draft-headers '(References))
  (add-to-list 'message-draft-headers '(In-Reply-To))
  ;;(add-to-list 'message-draft-headers '(User-Agent))
  (add-to-list 'message-required-mail-headers '(References))

  :init
  (setq mail-host-address "gnus.com")
  (setq sendmail-program "/usr/bin/msmtp")
  ;;(setq message-sendmail-extra-arguments
  ;;      '("--password" (auth-source-pass-get "")))

  (setq
   ;; this is handled by (check-mail). the problem with this confirmation is
   ;; that it always jumps to the end of the buffer, making it hard to review.
   ;;
   ;; message-confirm-send t

   message-send-mail-function 'message-send-mail-with-sendmail
   message-kill-buffer-on-exit t
   message-generate-headers-first t ;;'(References))

   ;; Default citation. I prefer inline, but gmail and it's users prefer
   ;; citation above for most replies.
   ;; message-citation-line-function 'message-insert-formatted-citation-line

   ;; Use the "From" field to determine the sender.
   message-sendmail-envelope-from 'header
   mail-envelope-from 'header
   mail-specify-envelope-from 'header))


(use-package mml
  :hook ((gnus-message-setup . mml-secure-message-sign-pgpmime))
  :init
  (setq
   ;; mime-edit-pgp-signers '("C84EF897")
   ;; mime-edit-pgp-encrypt-to-self t
   ;; mml2015-encrypt-to-self t
   ;; mml2015-sign-with-sender t
   ;; You need to replace this key ID with your own key ID!
   mml-secure-openpgp-signers '("ACBE1F5C")
   ;; We want to be able to read the emails we wrote.
   mml-secure-openpgp-encrypt-to-self t))


(use-package nndraft
  :config
  (add-to-list 'nndraft-required-headers '(References)))


(use-package epg
  :init
  (setq epg-user-id user-mail-address))


(use-package mm-decode
  :after (gnus)
  :init
  (setq
   mm-inline-text-html-with-images t
   mm-text-html-renderer 'gnus-w3m
   ;;mm-html-blocked-images nil
   ;;mm-discouraged-alternatives '("text/html" "text/richtext")
   ;;mm-sign-option 'guided
   ;;mm-encrypt-option 'guided
   mm-decrypt-option 'always
   mm-verify-option 'always))

(use-package gnus-icalendar
  :requires (gnus)
  :config
  (setq gnus-icalendar-org-capture-file "~/org/cal.org")
  (setq gnus-icalendar-org-capture-headline '("Calendar"))
  (require 'org-agenda)
  (gnus-icalendar-setup)
  (gnus-icalendar-org-setup))


(use-package org-mime
  :after (org)
  :ensure t
  :config
  (setq org-mime-export-options
        '(:section-numbers nil
                           :with-author nil
                           :with-toc nil))
  ;;(setq org-mime-find-html-start
  ;;    (lambda (start)
  ;;      (save-excursion
  ;;        (goto-char start)
  ;;        (search-forward "<#secure method=pgpmime mode=sign>")
  ;;        ;;(or (search-forward "<#secure method=pgpmime mode=sign>") (search-forward "--text below this line--"))
  ;;        (+ (point) 1))))
  ;;(setq org-mime-library 'mml)
  )

;;; .gnus ends here
