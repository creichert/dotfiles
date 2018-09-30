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
;;     reply to an email message is called reply to .
;;
;; Keybindings:
;;
;;   group mode (list of accounts, inboxes and labels):
;;     - g   : scan all servers for new articles
;;     - A s : show only subscribed mailboxes with new mail
;;     - L   : show all imap mailboxes
;;     - A A : show ALL mailboxes
;;     - C-k : kill subscription.  For nntp, this marks group as `K'.
;;
;;
;;   summary mode (list of emails):
;;     - M C-c : Mark all articles in group as read
;;     - d     : mark read
;;     - D     : delete permanently
;;     - E     : mark expired
;;     - B m   : Move article from one group to another
;;     - S w   : Reply "site-wide", e.g. to all To and Cc without quoting
;;     - `gnus-summary-insert-old-articles`	: view "read" articles
;;
;;   article (single mail view):
;;
;;     - r   : Reply-To no quote
;;
;;     - R   : Reply-To and quote
;;
;;     - S L : Reply-To List by TO'ing the list and drop cc's
;;
;;             - this option is intended to not send two copies of mails to
;;               someone you are replying to.  it's preferred to set a "to"
;;               address for the group in `gnus-parameters`, to avoid this
;;               problem.
;;
;;     - M-u : remove all readness marks from message
;;     - S W : Reply "site-wide", e.g. to all To and Cc and quote the original.
;;             fairly noisy reply.  Only do it when you are talking amongst a
;;             conversation and trying to reply to a person directly.
;;     - K v : View the Mime Part (attachment)
;;     - K o : View the Mime Part (attachment)
;;     - K O : View the Mime Part (attachment), prompt for filename
;;
;;   nntp / newsgroup "mode" settings (gmane, gwene, etc):
;;
;;    misc:
;;      - L L - show ALL groups
;;      - ^   - Select a source
;;      - u   - Subscribe / Unsubscribe to a newsgroup
;;
;;    message statuses:
;;      - U   - unsubscribed
;;      - ' ' - subscribed
;;      - K   - ??
;;
;; PGP / gpg signing
;;
;;    - A good PGP setup is to have a master key, encryption key, and additional
;;      subkeys where the master key is offline.  This has many benefits, all
;;      explained along with a how-to here:
;;
;;      - blog: https://alexcabal.com/creating-the-perfect-gpg-keypair/
;;      - script: https://gist.github.com/creichert/44060f9fd82607dc5f019e2b558edca0
;;      - removing master key: https://incenp.org/notes/2015/using-an-offline-gnupg-master-key.html
;;      - debian guide; http://keyring.debian.org/creating-key.html
;;
;;    Keybindings:
;;      - `C-c C-m s p' -> sign email via gpg
;;      - `C-c C-m c p' -> encrypt email via gpg
;;      - `;'           -> bbdb-mua-edit
;;
;;
;; Gnus resoures and config examples:
;;   - http://www.bobnewell.net/publish/35years/gmailhacks.html
;;   - https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;;   - http://qsdfgh.com/articles/gnus-configuration-example.html
;;   - https://lists.gnu.org/archive/html/help-gnu-emacs/2009-10/msg00307.html
;;   - https://github.com/vanicat/emacs24-starter-kit/blob/my-change/moi/gnus-config.org
;;   - http://sachachua.com/notebook/emacs/dotgnus.el
;;   - http://sachachua.com/blog/2008/05/emacs-gnus-searching-mail
;;   - http://blog.binchen.org/posts/notes-on-using-gnus.html
;;   - http://www.cataclysmicmutation.com/2010/11/multiple-gmail-accounts-in-gnus/
;;   - https://www.gnu.org/software/emacs/manual/html_node/gnus/ding-Gnus.html
;;   - http://www.emacswiki.org/emacs/GnusAttachmentReminder

;;; Code:

;; my gnus-private file contains various email accounts info.
;;
;; - gnus-parameters
;; - gnus-topic-topology
;; - gnus-select-method
;;
;;(load-file "~/.emacs.d/lisp/gnus-private.el")
(use-package gnus-private
  :load-path "lisp/")


(use-package gnus
  :after (gnus-private)
  :config
  (setq gnus-interactive-exit nil
        gnus-completing-read 'gnus-ido-completing-read
        gnus-ignored-mime-types '("text/x-vcard")
        gnus-asynchronous t
        gnus-treat-highlight-signature 'last
        gnus-large-newsgroup 25
        gnus-list-groups-with-ticked-articles nil
        gnus-use-dribble-file nil
        gnus-use-cache t
        gnus-button-url 'browse-url-browser-function
        mm-text-html-renderer 'gnus-w3m
        gnus-mime-view-all-parts t
        gnus-mime-display-multipart-related-as-mixed t
        gnus-default-charset 'utf-8
        gnus-default-posting-charset 'utf-8
        gnus-extra-headers '(To Newsgroups X-GM-LABELS)

        ;; only needed for compatibility w/ other mail readers
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil

        ;; Group line
        ;; gnus-group-line-format " %S [%5y] | %-20s | %G\n"
        ;; gnus-group-line-format "%P|%B|%M%o%S%L[%6t|%3i]%6y :%(%~(pad-right 65)g%):%6,6~(cut 2)d\n"
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
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)

        gnus-visible-headers
        '("^To" "^From" "^Subject" "^Date" "^Newsgroups" "^Followup-To"
          "^User-Agent" "^X-Newsreader" "^X-Mailer" "^Organization"
          "^X-Troll" "^Cc" "^Reply-To" "^Content-Type" "^X-Originating-IP"
          "^X-Priority" "^Message-ID" "^X-PGP-Key"
          ;;"^X-GMail-DKIM-Signature" "^DKIM-Signature" "^Resent-From"
          )

        gnus-signature-separator
        '("^-- $" "^-- *$" "^-------*$" "^ *--------*$" "^________*$" "^========*$")

        ;; Date specific format
        gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today, %H:%M")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
          (604800 . "%A %H:%M")
          ((gnus-seconds-month) . "%A %d")
          ((gnus-seconds-year) . "%d %B")
          (t . "%d/%m/%Y %H:%M"))

        ;; Gmail system labels have the prefix [Gmail], which matches the default
        ;; value of gnus-ignored-newsgroups. That's why we redefine it.
        ;; gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
        ;;
        ;; no local archives
        ;;gnus-message-archive-group nil
        ;;gnus-permanently-visible-groups "^nnimap\\+simplyrets:INBOX"
        ;;
        ;; gnus-topic-indent-level 3
        ;;
        ;; local archives split to folders
        ;;gnus-message-archive-group
        ;;    (("home" "nnimap+home:[Gmail]/All Mail")
        ;;     ("work" "nnimap+work:[Gmail]/All Mail")
        ;;     (".*" ,(format-time-string "sent/%Y-%m"))))
        ;;
        ;;gnus-check-new-newsgroups nil
        ;;gnus-check-bogus-newsgroups nil
        ;;gnus-auto-center-summary nil
        ;;gnus-nov-is-evil nil
        ;;gnus-show-threads t
        ;;gnus-use-cross-reference nil
        ;;
        ;; gnus-agent
        ;;
        ;; _might_ make imap slower. enabled by default
        ;;gnus-agent nil
        ;;gnus-agent-queue-mail t
        ;;gnus-agent-go-online t
        ;;gnus-inhibit-images nil
        ;;gnus-fetch-old-headers 'some
        ;;
        ;; gnus-sync-global-vars '(gnus-topic-topology gnus-topic-alist)
        ;; gnus-startup-file "~/.newsrc"
        ;; mail-source-directory        "~/.gnus.d/incoming/"
        ;; message-directory            "~/.gnus.d/mail/"
        ;; message-auto-save-directory  "~/.gnus.d/mail/drafts/"
        ;; nnmail-message-id-cache-file "~/.gnus.d/mail/.nnmail-cache"
        ;; nnml-newsgroups-file         "~/.gnus.d/mail/newsgroup"
        ;; nndraft-directory            "~/.gnus.d/mail/drafts"
        )
  (add-to-list
   'gnus-newsgroup-variables
   '(gnus-buttonized-mime-types . '("multipart/encrypted"
                                    "multipart/signed"
                                    "multipart/alternative")))

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
  ((gnus-summary-exit       . gnus-summary-bubble-group))

  ;; :custom-face
  ;; (set-face-attribute 'gnus-group-mail-1 t :foreground (x-get-resource "color2" ""))
  ;; (gnus-group-mail-1 ((t (:foreground (x-get-resource "color2" "")))))

  ((gnus-startup . (lambda ()
                     (split-window-horizontally)
                     (next-multiframe-window)
                     (info "Gnus")
                     ;; assuming f10 is bound to (jump-to-register 9) in .emacs,
                     ;; use [f10] to restore original {group|info} frames.
                     (window-configuration-to-register 9))))
  )

(use-package gnus-topic
  :hook ((gnus-group-mode . gnus-topic-mode))
  ;; :init
  ;; (setq-default gnus-topic-display-empty-topics t)
  ;; (setq-default gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics)
  ;; (setq-default gnus-subscribe-newsgroup-method 'my-gnus-subscribe-topics)
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
  :init
  ;; Message settings
  (setq
   ;; Add date to reply & quote
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "\nOn %a, %b %d %Y, %f wrote:"
   ;;message-forward-as-mime nil
   ;; add Cc and Bcc headers to the message buffer
   message-default-mail-headers "Cc: \nBcc: \n"

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
                (setq fill-column 80)
                (turn-on-auto-fill)))))

;;; contacts
(use-package bbdb
  :ensure t
  :commands (bbdb)
  :init
  (setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'message) ;; use 'gnus for incoming messages too

  :hook ((message-mode . (lambda () (local-set-key "<TAB>" 'bbdb-complete-name))))
  )

;;; .gnus ends here
