
(use-package slack
  :disabled
  :ensure t
  :defer
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "assertible"
   :default t
   :client-id "creichert07@gmail.com"
   :client-secret (auth-source-pass-get 'secret "slack/assertible/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/assertible/creichert")
   :subscribed-channels '(dev general notifications ops gh ci)
   :full-and-display-names t))

(provide 'slack-settings)

;;; slack-settings.el ends here
