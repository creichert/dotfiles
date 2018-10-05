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
;; (load-file "~/.emacs.d/lisp/gnus-private.el")
;;
;; only load if this file exists
(use-package gnus-private
  :if (file-exists-p "~/.emacs.d/lisp/gnus-private.el")
  :load-path "lisp/")


(use-package gnus

  :after (gnus-private)

  :config
  (setq gnus-interactive-exit nil
        gnus-completing-read 'gnus-ido-completing-read
        gnus-asynchronous t

        gnus-default-charset 'utf-8
        gnus-default-posting-charset 'utf-8

        gnus-large-newsgroup 100
        gnus-use-cache t
        gnus-button-url 'browse-url-browser-function

        ;; set in w3m config, not here
        mm-text-html-renderer 'gnus-w3m

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
        gnus-summary-line-format " %R%U%z %4k | %(%~(pad-right 16)&user-date; | %-25,25f | %B%s%)\n"

        ;; improve gmail support
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)


        ;;gnus-score-find-score-files-function
        ;;'(gnus-score-find-bnews.span class=compcode>bbdb/gnus-score)


        ;;gnus-treat-highlight-signature 'last
        ;;gnus-list-groups-with-ticked-articles nil
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

  (gnus-add-configuration
   '(article
     (horizontal 1.0
                 (vertical 33 (group 1.0))
                 (vertical 1.0
                           (summary 0.16 point)
                           (article 1.0)))))

  (gnus-add-configuration
   '(summary
     (horizontal 1.0
                 (vertical 33 (group 1.0))
                 (vertical 1.0 (summary 1.0 point)))))


  :hook
  ((gnus-select-group       . gnus-group-set-timestamp))
  ((gnus-after-exiting-gnus . kill-emacs))
  ;;((gnus-summary-exit       . gnus-summary-bubble-group))


  ((gnus-startup . (lambda ()
                     (split-window-horizontally)
                     (next-multiframe-window)
                     (info "Gnus")
                     ;; assuming f10 is bound to (jump-to-register 9) in .emacs,
                     ;; use [f10] to restore original {group|info} frames.
                     (window-configuration-to-register 9))))
  ;; (set-face-attribute 'gnus-group-mail-1 t :foreground (x-get-resource "color2" ""))
  ;;:custom-face
  ;;(gnus-group-mail-1 ((t (:foreground (x-get-resource "color2" "")))))
  )

(use-package gnus-sum
  :config
  (add-to-list
   'gnus-newsgroup-variables
   '(gnus-buttonized-mime-types . '("multipart/encrypted"
                                    "multipart/signed"
                                    "multipart/alternative"))))


(use-package gnus-srvr
  :bind (:map gnus-browse-mode-map
              ( "q" .  'gnus-browse-exit )))


(use-package gnus-topic
  :hook ((gnus-group-mode . gnus-topic-mode))
  :bind (:map gnus-topic-mode-map
              ("?\t" . gnus-topic-select-group))

  :init
  ;;(setq-default gnus-topic-display-empty-topics t)
  (setq-default gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics)
  (setq-default gnus-subscribe-newsgroup-method         'gnus-subscribe-topics)
  )


(use-package mml
  :hook ((gnus-message-setup . mml-secure-message-sign-pgpmime))
  :init
  (setq
   ;; You need to replace this key ID with your own key ID!
   mml-secure-openpgp-signers '("ACBE1F5C")
   ;; We want to be able to read the emails we wrote.
   mml-secure-openpgp-encrypt-to-self t))

(use-package epg
  :init
  (setq epg-user-id user-mail-address))

(use-package mm-decode
  :init
  (setq mm-inline-text-html-with-images t
        mm-discouraged-alternatives '("text/html" "text/richtext")
        ;;mm-text-html-renderer 'w3m
        ;;mm-w3m-safe-url-regexp nil
        mm-decrypt-option 'always
        mm-verify-option 'always))

;; Outgoing messages sent via msmtp (config in ~/.msmptrc)
;;
;; TODO: use smtpmail-send-it (depends on gnutls-cli)
;;
;; (setq
;;   message-send-mail-function 'smtpmail-send-it
;;   send-mail-funtion 'smtpmail-send-it
;;   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;   ;smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
;;   smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-mail-address nil))
;;   smtpmail-default-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-service 587
;;   smtpmail-debug-info t)
;;
(use-package message
  :bind (:map message-mode-map
              ( "\t"  . bbdb-complete-mail ))
  :init
  (setq vc-follow-symlinks t)
  ;; Message settings
  (setq
   ;; Add date to reply & quote
   message-confirm-send t
   message-citation-line-function 'message-insert-formatted-citation-line
   message-send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "/usr/bin/msmtp"
   smtpmail-use-starttls t
   )
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (flyspell-mode 1)))

  (unless (boundp 'message-fill-column)
    (add-hook 'message-mode-hook
              (lambda ()
                (setq fill-column 100)
                (turn-on-auto-fill)))))


;;; .gnus ends here
