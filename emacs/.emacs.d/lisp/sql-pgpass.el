

(use-package sql
  :hook  ((sql-mode . populate-sql-connection-alist))
  :preface
  ;; Add new servers to the `sql-connection-alist` for every server found in
  ;; your "~/.pgpass" file.
  (defun populate-sql-connection-alist ()
    (setq sql-connection-alist (pgpass-to-sql-connection (read-file "~/.pgpass"))))
  :init
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
