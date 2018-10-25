
;;; Code:

(use-package sql
  :demand
  :hook
  ((sql-mode       . populate-sql-connection-alist))
  ((sql-login-hook . sql-setup-postgresql))
  ((sql-set-sqli   . sql-setup-postgresql))
  :preface
  ;; Add new servers to the `sql-connection-alist` for every server found in
  ;; your "~/.pgpass" file.
  (defun populate-sql-connection-alist ()
    (setq sql-connection-alist (pgpass-to-sql-connection (read-file "~/.pgpass"))))
  (defun sql-setup-postgresql ()
    (sql-send-string "\\x\n")
    (sql-send-string "\\set ECHO queries\n"))
  :init

  (setq-default sql-production-connection-regexp "production")

  (defadvice sql-send-string (around sql-send-prod-y-or-n-p)
    (save-excursion
      ;; Set product context
      (with-current-buffer sql-buffer
        ;; Send the string (trim the trailing whitespace)
        (message "sql-send-paragraph")
        (message (format "%s" sql-connection))
        (cond
         ((string-match sql-production-connection-regexp sql-connection)
               ;;(not (string-match "^\\n\\|^\\set ECHO queries" str))
          (when (y-or-n-p "Send statement to PRODUCTION host? ")
            ad-do-it))
         ;; if not a production host, just run it
         (t ad-do-it)))))

  (ad-activate 'sql-send-string)

  ;; .pgpass parser
  (defun read-file (file)
    "Returns file as list of lines."
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t)))

  (defun pgpass-to-sql-connection (config)
    "Returns a suitable list for sql-connection-alist from a pgpass file."
    (append sql-connection-alist
            (let ((server (lambda (host port db user _pass)
                            (list
                             (concat db ":" user ":" port ":" host)
                             (list 'sql-product ''postgres)
                             (list 'sql-server host)
                             (list 'sql-user user)
                             (list 'sql-port (string-to-number port))
                             (list 'sql-database db))))
                  (pgpass-line (lambda (line)
                                 (apply server (split-string line ":" t)))))
              (mapcar pgpass-line config)))))


(provide 'sql-pgpass)
