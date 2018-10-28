

(use-package slack
  :ensure t
  :defer
  :commands (slack-start)
  :bind (("C-c C-b" . slack-select-unread-rooms)
         ("C-c C-t" . slack-change-current-team)
         ("C-c s i" . slack-im-select)
         ("C-c s u" . slack-select-unread-rooms)
         ("C-c C-j" . slack-channel-select))
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config

  ;; Using this globally currently doesn't work w/ gnus
  (use-package auth-source-pass
    :ensure t
    :init
    (auth-source-pass-enable))
  (require 'auth-source-pass)

  (slack-register-team
   :name "SimplyRETS"
   :client-id     (auth-source-pass-get "user" "slack/simplyrets/creichert")
   :client-secret (auth-source-pass-get 'secret "slack/simplyrets/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/simplyrets/creichert")
   ;;:subscribed-channels '(dev general notifications ops gh ci)
   :full-and-display-names t)

  (slack-register-team
   :name "Assertible"
   :default t
   :client-id     (auth-source-pass-get "user" "slack/assertible/creichert")
   :client-secret (auth-source-pass-get 'secret "slack/assertible/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/assertible/creichert")
   :subscribed-channels '(dev general notifications ops gh ci)
   :full-and-display-names t)

  (defun creichert/slack-mode--catch-message-to-string-error (orig-fun &rest args)
    (condition-case nil
        (apply orig-fun args)
      (error "<error parsing message>\n")))
    (advice-add 'slack-message-to-string :around #'creichert/slack-mode--catch-message-to-string-error))

(provide 'slack-settings)

;;; slack-settings.el ends here
