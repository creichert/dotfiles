
(require 'cl)

(use-package slack
  :ensure t
  :commands (slack-start)
  :hook
  ((lui-mode . creichert/lui-setup))
  ((lui-mode . turn-on-auto-revert-mode))
  :bind (("C-c s s" . slack-start)
         ("C-c s j" . slack-select-rooms)
         (:map slack-mode-map
               ;;primary commands (similar to erc)
               ;;("C-c C-b" . creichert/slack-select-unreads)
               ("C-c C-b" . slack-select-unread-rooms)
               ("C-c C-t" . slack-change-current-team)
               ("C-c C-j" . slack-select-rooms)
               ("C-c C-p" . slack-buffer-goto-prev-message)
               ("C-c C-n" . slack-buffer-goto-next-message)
               ;; prefix commands
               ("C-c s i" . slack-im-select)
               ("C-c s e" . slack-message-edit)
               ("C-c s t" . slack-change-current-team)
               ("C-c s u" . slack-select-unread-rooms)
               ("C-c s r" . slack-message-add-reaction)
               ("C-c s N" . slack-message-test-notification)
               ;; special commands
               ("C-c s @" . creichert/slack-message-embed-mention)
               ("C-c s #" . creichert/slack-message-embed-channel)
               ("C-c s :" . creichert/slack-message-embed-emoji)))
  :preface
  (defun creichert/slack-message-embed-mention ()
    (interactive)
    (call-interactively #'slack-message-embed-mention)
    (insert " "))
  (defun creichert/slack-message-embed-channel ()
    (interactive)
    (call-interactively #'slack-message-embed-channel)
    (insert " "))
  (defun creichert/slack-message-embed-emoji ()
    (interactive)
    (call-interactively #'emojify-insert-emoji)
    (insert " "))
  (defun creichert/slack-select-unreads-all-teams ()
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
  (setq
   lui-time-stamp-position 'right-margin
   lui-fill-type nil)

  (defun creichert/slack-mode--catch-message-to-string-error (orig-fun &rest args)
    (condition-case nil
        (apply orig-fun args)
      (error "<error parsing message>\n")))
  (advice-add 'slack-message-to-string :around #'creichert/slack-mode--catch-message-to-string-error)

  (defun creichert/slack-start (orig-fun &rest args)
    (apply orig-fun args)
    (sit-for 10)
    (slack-room-display
     (slack-room-find-by-name "dev" slack-current-team)
     slack-current-team))

  (advice-add 'slack-start :around #'creichert/slack-start)

  :config

  ;; Using this globally currently doesn't work w/ gnus
  (use-package auth-source-pass :ensure t :demand)

  (slack-register-team
   :name "SimplyRETS"
   :client-id     (auth-source-pass-get "user" "slack/simplyrets/creichert")
   :client-secret (auth-source-pass-get 'secret "slack/simplyrets/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/simplyrets/creichert")
   :subscribed-channels '(dev simplyrets ci ops general random)
   :full-and-display-names t)

  (slack-register-team
   :name "Assertible"
   :default t
   :client-id     (auth-source-pass-get "user" "slack/assertible/creichert")
   :client-secret (auth-source-pass-get 'secret "slack/assertible/creichert")
   :token         (auth-source-pass-get "legacy-token" "slack/assertible/creichert")
   ;; send notifications to minibuffer / higher alert importance
   :subscribed-channels '(dev gh notifications support ci general random)
   :full-and-display-names t)

  (use-package alert
    :ensure t
    :config

    ;; notify all messages in these channels.
    (add-to-list
     'alert-user-configuration
     '((;;(:title . "Christopher")
        ;;(:status '(buried idle))
        (:category . "slack"))
       libnotify nil))
    ))

(provide 'slack-settings)

;;; slack-settings.el ends here
