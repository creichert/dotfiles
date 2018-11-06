

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
  ;;(defun creichert/slack-select-unreads-all-teams ()
  ;;  (interactive)
  ;;  (let ((team (slack-team-select)))
  ;;    (slack-room-select
  ;;     (cl-loop for team in (list team)
  ;;              append (with-slots (groups ims channels) team
  ;;                       (cl-remove-if #'(lambda (room) (not (< 0 (oref room unread-count-display))))
  ;;                                     (append ims groups channels)))))))
  (defun creichert/lui-setup ()
    (interactive)
    (setq
     ;;fringes-outside-margins t
     right-margin-width 7
     word-wrap t
     wrap-prefix "    ")
    (setq-local alert-default-style 'libnotify))
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  (setq slack-completing-read-function #'ido-completing-read)
  :config
  (setq
   lui-time-stamp-position 'right-margin
   lui-fill-type nil)

  ;; (defun creichert/slack-mode--catch-message-to-string-error (orig-fun &rest args)
  ;;   (condition-case nil
  ;;       (apply orig-fun args)
  ;;     (error "<error parsing message>\n")))
  ;; (advice-add 'slack-message-to-string :around #'creichert/slack-mode--catch-message-to-string-error)

  (defun creichert/slack-start (orig-fun &rest args)
    (apply orig-fun args)
    ;;(sit-for 5)
    ;;(slack-room-display
    ;; (slack-room-find-by-name "dev" slack-current-team)
    ;; slack-current-team)
    )
  (advice-add 'slack-start :around #'creichert/slack-start)

  (use-package color :demand)
  (use-package alert
    :ensure-system-package (dunst)
    :ensure t
    :config
    ;; notify via libnotify
    (add-to-list 'alert-user-configuration '(((:category . "slack")) libnotify nil))

    (defvar subscribed-channel-regexp "\\(z-.*\\|dev\\|ops\\|simplyrets\\|ci\\|ops\\|general\\|random\\|support\\|gh\\|identibyte\\)")
    (defvar subscribed-keyword-regexp "\\(chris\\|failure\\|deployed.*production\\|Failed.*creichert\\)")
    (defvar subscribed-user-regexp "\\(codyreichert\\|github\\)")
    (defvar subscribed-urgent-keyword-regexp "\\(High.*IOPS\\|production.*tests.*failed.*via\\)")
    (defvar subscribed-urgent-user-regexp "\\(cody\\|drift\\)")
    (defvar subscribed-urgent-channel-regexp "\\(z-.*\\|identibyte\\)")

    ;; iterate attachment fields to identify urgency
    ;; (defvar subscribed-urgent-attachment-field-regexp "\\(High.*IOPS\\|production.*tests.*failed.*via\\)")
    (defvar ignored-user-regexp "MailClark")
    (setq slack-message-custom-notifier
          #'(lambda (message room team)
              (cond ((and (not (slack-message-minep message team))
                          (not (string-match ignored-user-regexp (slack-message-sender-name message team)))
                          ;; MATCH a keyword or channel to get alerts
                          (or (string-match subscribed-channel-regexp (slack-room-name room team))
                              (string-match subscribed-keyword-regexp (slack-message-to-alert message team))
                              (string-match subscribed-user-regexp (slack-message-sender-name message team))
                              (string-match subscribed-urgent-user-regexp (slack-message-sender-name message team))
                              (string-match subscribed-urgent-keyword-regexp (slack-message-to-alert message team))
                              (string-match subscribed-urgent-channel-regexp (slack-room-name room team))
                              ))
                     (let* ((room-name (slack-room-name room team))
                            (text (slack-message-to-alert message team))
                            (user-name (slack-message-sender-name message team))
                            (is-urgent (or (string-match subscribed-urgent-user-regexp user-name)
                                           (string-match subscribed-urgent-keyword-regexp text)
                                           (string-match subscribed-urgent-channel-regexp room-name))))
                       ;;(if (and (eq alert-default-style 'notifier)
                       ;;         (or (eq (aref text 0) ?\[)
                       ;;             (eq (aref text 0) ?\{)
                       ;;             (eq (aref text 0) ?\<)
                       ;;             (eq (aref text 0) ?\()))
                       ;;    (setq text (concat "\\" text)))
                       (alert text
                              :title (format "[%s] from %s" room-name user-name)
                              :severity (cond (is-urgent 'urgent)
                                              (t 'normal))
                              :persistent
                              #'(lambda (info)
                                  ;; If the buffer is buried, or the user has
                                  ;; been idle for `alert-reveal-idle-time'
                                  ;; seconds, make this alert persistent.
                                  ;; Normally, alerts become persistent after
                                  ;; `alert-persist-idle-time' seconds.
                                  (and (not (memq (plist-get info :status) '(visible selected)))
                                       is-urgent))
                              :category 'slack)))
                    (t (slack-message-notify-alert message room team)))))

    ;; Using this globally currently doesn't work w/ gnus
    (use-package auth-source-pass :ensure t :demand)

    (slack-register-team
     :name "SimplyRETS"
     :client-id     (auth-source-pass-get "user" "slack/simplyrets/creichert")
     :client-secret (auth-source-pass-get 'secret "slack/simplyrets/creichert")
     :token         (auth-source-pass-get "legacy-token" "slack/simplyrets/creichert")
     ;; :subscribed-channels '(dev simplyrets ci ops general random)
     :full-and-display-names t)

    (slack-register-team
     :name "Assertible"
     :default t
     :client-id     (auth-source-pass-get "user" "slack/assertible/creichert")
     :client-secret (auth-source-pass-get 'secret "slack/assertible/creichert")
     :token         (auth-source-pass-get "legacy-token" "slack/assertible/creichert")
     ;; send notifications to minibuffer / higher alert importance
     ;;:subscribed-channels '(dev gh notifications support ci general random)
     :full-and-display-names t)
    ))

(provide 'slack-settings)

;;; slack-settings.el ends here
