
;; Basic webpack-dev-server support in emacs with compilation-mode
;;
;; Use M-x webpack-dev-server to launch

(setq webpack-dev-server-height 30)

(setq webpack-dev-server-buf-name "*webpack-dev-server*")

(define-minor-mode webpack-dev-server-mode
  "A minor mode for webpack-dev-server terminals"
  :lighter " Webpack-Dev-Server"
  ;;(nlinum-mode -1)
  (linum-mode -1)
  ;; add error regexp for webpack errors
  (add-to-list 'compilation-error-regexp-alist-alist
               '(webpack "\\(?:ERROR\\|\\(WARNING\\)\\).* \\(at\\|on\\|in\\) \\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\)"
                           3 ;; file
                           4 ;; line
                           nil ;; column
                           (0 . 1)))
  (add-to-list 'compilation-error-regexp-alist 'webpack)
  (compilation-minor-mode))

(defun new-webpack-dev-server-term ()
  (interactive)
  (kill-webpack-dev-server)
  (let ((webpack-dev-server-buf (get-buffer-create webpack-dev-server-buf-name)))
    (display-buffer
     webpack-dev-server-buf
     '((display-buffer-at-bottom
        display-buffer-pop-up-window
        display-buffer-reuse-window)
       (window-height . 30)))
    (select-window (get-buffer-window webpack-dev-server-buf))
    (make-term "webpack-dev-server" "/bin/bash")
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (setq-local term-buffer-maximum-size webpack-dev-server-height)
    (setq-local scroll-down-aggressively 1)
    (setq-local compilation-scroll-output 'first-error)
    (webpack-dev-server-mode)))

(defun kill-webpack-dev-server ()
  (let* ((webpack-dev-server-buf (get-buffer webpack-dev-server-buf-name))
         (webpack-dev-server-proc (get-buffer-process webpack-dev-server-buf)))
    (when (processp webpack-dev-server-proc)
      (progn
        (set-process-query-on-exit-flag webpack-dev-server-proc nil)
        (kill-process webpack-dev-server-proc)))))

(defun add-stars (s) (format "*%s*" s))

(defun webpack-dev-server-command (h)
  (format "make frontend-dev\n"))

;; TODO Close stuff if it fails
(defun webpack-dev-server ()
  "Run webpack-dev-server"
  (interactive)
  (let ((cur (selected-window)))
    (new-webpack-dev-server-term)
    (comint-send-string webpack-dev-server-buf-name (webpack-dev-server-command webpack-dev-server-height))
    (select-window cur)))

;; Assumes that only one window is open
(defun webpack-dev-server-stop ()
  "Stop webpack-dev-server"
  (interactive)
  (let* ((webpack-dev-server-buf (get-buffer webpack-dev-server-buf-name))
         (webpack-dev-server-window (get-buffer-window webpack-dev-server-buf)))
    (when webpack-dev-server-buf
      (progn
        (kill-webpack-dev-server)
        (select-window webpack-dev-server-window)
        (kill-buffer-and-window)))))

(provide 'webpack-dev-server)
