

(use-package bbdb
  :ensure t
  :commands (bbdb)
  :bind (:map bbdb-mode-map
              ("\t"      . bbdb-complete-mail )
              ("C-c b s" . bbdb-display-current-record)
              ("C-c b l" . bbdb-toggle-records-layout)
              ("C-x b k" . bbdb-delete-field-or-record)
              ("C-x C-s" . bbdb-save)
              :map message-mode-map
              ( "\t"      . bbdb-complete-mail )
              ( "C-c b l" . bbdb-toggle-records-layout))
  :hook ((mail-setup       . bbdb-mail-aliases)
         (message-setup    . bbdb-mail-aliases)
         (bbdb-create      . bbdb-save)
         (bbdb-notice-mail . bbdb-auto-notes))
  :custom
  (bbdb-message-all-addresses t)
  (bbdb-complete-mail-allow-cycling t)
  (bbdb-case-fold-search t)
  (bbdb-offer-save t)
  (bbdb-mua-pop-up 'horiz)
  (bbdb-phone-style nil)
  (bbdb-message-all-addresses t)
  (bbdb-check-auto-save-file t)
  ;; (bbdb-user-mail-address-re "")
  :init
  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-update-records-p 'query)
  (setq bbdb-mua-auto-update-p 'search)
  (setq bbdb-notice-record-hook 'bbdb-auto-notes)
  (setq bbdb-add-aka 'query)
  ;; Rules for annotating records based on mua activity:
  ;;
  ;; structure:
  ;;   - (MUA FROM-TO HEADER ANNOTATE ...)
  ;;   - (FROM-TO HEADER ANNOTATE ...)
  ;;   - (HEADER ANNOTATE ...)
  ;;
  ;; ANNOTATE may take the following values:
  ;;
  ;; (REGEXP . STRING)       [this is equivalent to (REGEXP notes STRING)]
  ;; (REGEXP FIELD STRING)
  ;; (REGEXP FIELD STRING REPLACE)
  ;;
  ;; If REPLACE is t, the resulting string replaces the old contents of FIELD.
  ;; If it is nil, the string is appended to the contents of FIELD (unless the
  ;; annotation is already part of the content of field).
  (setq bbdb-auto-notes-rules
        '(("X-Face" (".+" x-face 0 'replace))
          ("Face"   (".+" face 0 'replace))
          ("Organization" (".*" organization "\\1" nil))
          ("Newsgroups" ("[^,]+" newsgroups identity nil))
          ("Xref" ("[^ ]+ \\([^ :]+\\):[0-9]+" newsgroups "\\1" nil))
          ("User-Agent" (".*" mailer identity nil))
          ("X-Mailer" (".*" mailer identity nil))
          ("X-Newsreader" (".*" mailer identity nil))
          ("X-GitHub-Sender" (".*" github identity t))
          ("X-Google-Sender-Delegation" (".*" delegated identity nil))
          ))
  (setq bbdb-auto-notes-ignore-headers
        '((("Organization" . "^Gatewayed from\\|^Source only"))))
  (setq bbdb-ignore-message-alist
        '(;;(("Newsgroups") . "gmane.*")
          ;;(("From" . "mailer-daemon\\|no-reply.*\\|no.*reply\\|DAEMON\\|daemon")
          ;; (("To" "CC") . "mailing-list-1\\|mailing-list-2"))
          ))
  :config
  (bbdb-initialize 'gnus 'message 'anniv)
  (bbdb-mua-auto-update-init 'gnus 'message 'rmail))

(provide 'bbdb-settings)

;;; bbdb-settings.el ends here
