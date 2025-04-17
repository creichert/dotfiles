
;;; Code:

(require 'use-package)

(use-package gptel
  :ensure t
  :defer
  :config
  (gptel-make-xai "xAI"
                  :stream t
                  :key (auth-source-pass-get 'secret "x/xai/creichert07"))

  ;;(gptel-make-gemini "Gemini" :key "YOUR_GEMINI_API_KEY" :stream t)

  ;; (gptel-make-openai "Groq"
  ;;   :host "api.groq.com"
  ;;   :endpoint "/openai/v1/chat/completions"
  ;;   :key gptel-api-key
  ;;   :stream t
  ;;   :models '(deepseek-r1-distill-llama-70b llama-3.3-70b-versatile))

  (setq gptel-default-mode 'markdown-mode
	gptel-backend (gptel-get-backend "xAI")
	;gptel-model 'deepseek-r1-distill-llama-70b)
	gptel-model 'grok-3-latest)

  ;; auto scroll
  ;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

  ;; move cursor to next prompt
  ;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :preface
  ; remove the default model entries
  (with-eval-after-load 'gptel
    (setf (gptel-get-backend "ChatGPT") nil))
  )

;; (use-package dockerfile-mode
;;   :ensure t
;;   :ensure-system-package (docker . "docker.io")
;;   :config
;;   (put 'dockerfile-image-name 'safe-local-variable #'stringp)
;;   (put 'dockerfile-build-args 'safe-local-variable #'sequencep)
;;   :mode ("\\.Dockerfile.\\'" . dockerfile-mode))
;;
;;
;; (use-package docker
;;   :ensure t
;;   :commands (docker)
;;   :ensure-system-package (docker . "docker.io")
;;   :bind (("C-c d" . docker)))
;;
;;
;; (use-package ggtags
;;   :defer
;;   :requires (evil)
;;   :ensure t
;;   :commands (ggtags-mode
;;              ggtags-find-reference
;;              ggtags-idutils-query
;;              pop-tag-mark)
;;   :ensure-system-package
;;   ((gtags    . "global")
;;    ;;(pip      . "pip install pygments")
;;    (pygments . "pip install pygments"))
;;   :init
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (ggtags-mode 1))))
;;   :config
;;   (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
;;   :bind (
;;          ("M-," . pop-tag-mark)
;;          ("M-/" . ggtags-find-reference)
;;          ("M-]" . ggtags-idutils-query)
;;          :map ggtags-navigation-map
;;          ("M-u" . ggtags-navigation-previous-file)
;;          ("M-o" . ggtags-navigation-next-file)
;;          ("M-l" . ggtags-navigation-visible-mode)
;;          ("M-j" . ggtags-navigation-visible-mode)
;;          ("M-k" . next-error)
;;          ("M-i" . previous-error)))
;;
;;
;; (use-package rainbow-mode
;;   :defer
;;   :ensure t)
;;
;;
;; (use-package etags-select
;;   :disabled
;;   :load-path "site-lisp/etags-select.el/")
;;
;;
;;
;; (use-package ledger-mode
;;   :ensure-system-package (ledger)
;;   :ensure t
;;   :defer)
;;
;;
;; (use-package flycheck-ledger
;;   :disabled
;;   :ensure t
;;   :after (flycheck ledger-mode)
;;   :hook  ((ledger-mode . flycheck-ledger-mode)))
;;
;;
;; (use-package gif-screencast
;;   :defer
;;   :ensure t
;;   :if window-system
;;   ;;:ensure-system-package (gifsicle
;;   ;;                        mogrify
;;   ;;                        imagemagick)
;;   :init
;;   (setq gif-screencast-screenshot-directory "~/downloads/screencasts/tmp")
;;   (setq gif-screencast-output-directory "~/downloads/screencasts")
;;   :bind
;;   (([f11] . gif-screencast)
;;    ([f12] . gif-screencast-stop)))
;;
;;
;; (use-package term
;;   :commands (ssh-term)
;;   ;;:ensure-system-package (ssh)
;;   :init
;;   (defun remote-term (new-buffer-name cmd &rest switches)
;;     (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
;;     (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
;;     (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
;;     (set-buffer term-ansi-buffer-name)
;;     (term-mode)
;;     (term-char-mode)
;;     (term-set-escape-char ?\C-x)
;;     (switch-to-buffer term-ansi-buffer-name))
;;
;;   (defun ssh-term (host)
;;     (interactive "sHost: \n")
;;     (remote-term (format "ssh-%s" host) "ssh" (format "%s" host))))
;;
;;
;; (use-package alert
;;   :defer
;;   :ensure t)
;;
;;
;; (use-package noflet
;;   :defer
;;   :ensure t)
;;
;;
;; (use-package kill-ring-ido
;;   :requires (ido noflet)
;;   :custom (kill-ring-ido-shortage-length 40) ; where 6 is your value
;;   :bind (("C-c k" . kill-ring-ido))
;;   :load-path "site-lisp/")
;;
;;
;; (use-package pdf-tools
;;   :disabled
;;   ;;:load-path "site-lisp/pdf-tools/lisp"
;;   :magic ("%PDF" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install))
;;
;;
;; (use-package yasnippet
;;   :defer
;;   :ensure t
;;   :after (evil-leader)
;;   :hook ((haskell-mode . yas-minor-mode))
;;   :config
;;   (use-package yasnippet-snippets
;;     :disabled
;;     :ensure t)
;;   (use-package haskell-snippets
;;     :ensure t)
;;   ;;(yas-global-mode t)
;;   (yas-reload-all)
;;   (setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
;;   (evil-leader/set-key-for-mode 'magit-status-mode
;;     "SPC" 'yas-expand-maybe)
;;   :diminish yas-minor-mode)
;;
;;
;; (use-package slack-settings
;;   :load-path "lisp"
;;   :if (file-exists-p "~/.emacs.d/lisp/slack-settings.el"))
;;
;;
;; (use-package erc-settings
;;   :load-path "lisp"
;;   :if (file-exists-p "~/.emacs.d/lisp/erc-settings.el"))


(provide 'extra)

;;; extra.el ends here
