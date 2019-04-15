
(use-package erc
  :commands (erc erc-creichert)
  :defer
  :hook
  ((erc-mode . erc-spelling-mode)
   (erc-mode . (lambda ()
                 (set (make-local-variable 'scroll-conservatively) 100))))
  :custom
  (erc-user-full-name "creichert")
  (erc-nick-uniquifier "_")
  (erc-autojoin-timing 'ident)
  (erc-server-auto-reconnect t)
  (erc-prompt-for-nickserv-password nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))

  :preface
  (defun erc-creichert ()
    (interactive)
    (use-package auth-source-pass
      :ensure t :demand
      :init (auth-source-pass-enable))
    (erc-tls :server "irc.freenode.net" :port 6697 :nick "creichert"))

  :config
  (erc-track-minor-mode 1)
  (erc-track-mode 1)

  :init
  (setq
   erc-log-channels-directory "~/.emacs.d/erc/logs/"
   ;; erc-log-write-after-insert t

   erc-auto-discard-away t
   erc-input-line-position -2

   ;; todo
   ;; scroll-preserve-screen-position t
   ;; scroll-conservatively 0

   ;;Kill buffers for channels after /part
   ;;erc-kill-buffer-on-part t

   ;;Kill buffers for private queries after quitting the server
   ;;erc-kill-queries-on-quit t

   ;;Kill buffers for server messages after quitting the server
   ;;erc-kill-server-buffer-on-quit t

   ;; utf-8 always and forever
   erc-server-coding-system '(utf-8 . utf-8)

   ;;TEST: Interpret mIRC-style color commands in IRC chats
   erc-interpret-mirc-color t

   ;;If someone sends a /notice don't just show it in the server buffer,
   ;;but also in the mini-buffer.
   erc-echo-notices-in-minibuffer-flag t
   erc-button-url-regexp
   "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"
   erc-autojoin-channels-alist
   '((".*\\.freenode.net"
      "#hlug"
      "#haskell"
      "#emacs"
      "#debian"
      "#gnus"
      "#xmonad"
      "#yesod"
      ))))


(provide 'erc-settings)

;;; erc-settings.el ends here
