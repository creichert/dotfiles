(require 'use-package)

(use-package gif-screencast
  :ensure t
  :if window-system
  :init
  (setq gif-screencast-screenshot-directory "~/downloads/screencasts/tmp")
  (setq gif-screencast-output-directory "~/downloads/screencasts")
  :bind
  (([f11] . gif-screencast)
   ([f12] . gif-screencast-stop)))


(use-package password-store
  :ensure t)

(use-package magit-gh-pulls
  :ensure t
  :hook (magit-status-mode . magit-gh-pulls-mode)
  :init
  (setq magit-gh-pulls-pull-detail-limit 30)
  (setq magit-gh-pulls-status-documentation t))


(use-package term
  :commands (make-term term ssh-shell)
  :config
    (defun remote-term (new-buffer-name cmd &rest switches)
      (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
      (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
      (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
      (set-buffer term-ansi-buffer-name)
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x)
      (switch-to-buffer term-ansi-buffer-name))

    (defun ssh-shell (host)
      (interactive "sHost: \n")
      (remote-term (format "ssh-%s" host) "ssh" (format "%s" host))))


(provide 'extra)
