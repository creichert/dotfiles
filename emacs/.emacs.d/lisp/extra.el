
;;; Code:

(require 'use-package)

(use-package gptel
  :ensure t
  :defer
  :config
  (gptel-make-xai "xAI"
    :stream t
    :key (auth-source-pass-get 'secret "x/xai/creichert07"))

  (gptel-make-gemini "Gemini"
    :stream t
    :key (auth-source-pass-get 'secret "aistudio.google.com/rbros/api-token"))

  ;; (gptel-make-ollama "Ollama"
  ;; :host "localhost:11434"
  ;; :stream t
  ;; :models '(mistral:latest))

  (setq gptel-default-mode 'markdown-mode
	gptel-backend (gptel-get-backend "xAI")
        ;gptel-response-separator "\n\n------------------------------------------------\n\n"
	;gptel-model 'deepseek-r1-distill-llama-70b
	gptel-model 'grok-3-latest)

  ;; auto scroll
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

  ;; move cursor to next prompt
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  :preface
  ; remove the default model entries
  (with-eval-after-load 'gptel
    (setf (gptel-get-backend "ChatGPT") nil)))


(use-package dockerfile-mode
  :defer
  :ensure t
  ;:ensure-system-package (docker . "docker.io")
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  (put 'dockerfile-build-args 'safe-local-variable #'sequencep)
  :mode ("\\.Dockerfile.\\'" . dockerfile-mode))


; set background color when rgb text is discovered
(use-package rainbow-mode
  :defer
  :ensure t)


(provide 'extra)

;;; extra.el ends here
