

(use-package slack
  :ensure t
  :defer
  :commands (slack-start)
  :hook ((lui-mode . creichert/lui-setup))
  :bind (:map slack-mode-map
              ;;primary commands (similar to erc)
              ;;("C-c C-b" . creichert/slack-select-unreads)
              ("C-c C-b" . slack-select-unread-rooms)
              ("C-c C-t" . slack-change-current-team)
              ("C-c C-j" . slack-channel-select)
              ;; prefix commands
              ("C-c s i" . slack-im-select)
              ("C-c s t" . slack-change-current-team)
              ("C-c s u" . slack-select-unread-rooms)
              ;; special commands
              ("@" . creichert/slack-message-embed-mention)
              ("#" . creichert/slack-message-embed-channel))
  :preface
  (defun creichert/slack-message-embed-mention ()
    (interactive)
    (call-interactively #'slack-message-embed-mention)
    (insert " "))
  (defun creichert/slack-message-embed-channel ()
    (interactive)
    (call-interactively #'slack-message-embed-channel)
    (insert " "))
  (defun creichert/slack-select-unreads ()
    (interactive)
    (let ((team (slack-team-select)))
      (slack-room-select
       (cl-loop for team in (list team)
                append (with-slots (groups ims channels) team
                         (cl-remove-if #'(lambda (room) (not (< 0 (oref room unread-count-display))))
                                       (append ims groups channels)))))))

  (defun creichert/lui-setup ()
    (interactive)
    (setq
     fringes-outside-margins t
     right-margin-width 5
     word-wrap t
     wrap-prefix "    "))
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  (setq slack-completing-read-function #'ido-completing-read)
  (setq slack-buffer-function #'switch-to-buffer)
  :config
  ;; Using this globally currently doesn't work w/ gnus
  (use-package auth-source-pass
    :ensure t
    :demand
    :init
    (auth-source-pass-enable))
  (require 'auth-source-pass)

  (slack-register-team
   :name "SimplyRETS"
   :client-id     (auth-source-pass-get "user" "slack/simplyrets/creichert")
   :client-secret (auth-source-pass-get 'secret "slack/simplyrets/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/simplyrets/creichert")
   :full-and-display-names t)

  (slack-register-team
   :name "Assertible"
   :default t
   :client-id     (auth-source-pass-get "user" "slack/assertible/creichert")
   :client-secret (auth-source-pass-get 'secret "slack/assertible/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/assertible/creichert")
   ;; send notifications to minibuffer / higher alert importance
   ;;:subscribed-channels '(dev)
   :full-and-display-names t)

  (setq
   lui-time-stamp-position 'right-margin
   lui-fill-type nil)

  (defun creichert/slack-mode--catch-message-to-string-error (orig-fun &rest args)
    (condition-case nil
        (apply orig-fun args)
      (error "<error parsing message>\n")))
  (advice-add 'slack-message-to-string :around #'creichert/slack-mode--catch-message-to-string-error))



(provide 'slack-settings)

;;; slack-settings.el ends here
