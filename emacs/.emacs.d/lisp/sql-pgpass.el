
;;; Code:

(use-package sql
  :hook
  ((sql-mode . populate-sql-connection-alist))
  :preface
  ;; Add new servers to the `sql-connection-alist` for every server found in
  ;; your "~/.pgpass" file.
  (defun populate-sql-connection-alist ()
    (setq sql-connection-alist (pgpass-to-sql-connection (read-file "~/.pgpass"))))

  :init
  (setq-default sql-production-connection-regexp "production")

  (defun my-sql-send-prod-y-or-n-p (orig-fun &rest args)
    "Advice for `sql-send-string' to prompt before sending to production."
    (save-excursion
      ;; Set product context
      (with-current-buffer sql-buffer
        ;; Send the string (trim the trailing whitespace)
        (message "sql-send-paragraph")
        (message (format "%s" sql-connection))
        (cond
         ((string-match sql-production-connection-regexp sql-connection)
          (when (y-or-n-p "Send statement to PRODUCTION host? ")
            (apply orig-fun args)))
         ;; If not a production host, just run it
         (t (apply orig-fun args))))))

  (advice-add 'sql-send-string :around #'my-sql-send-prod-y-or-n-p)

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
