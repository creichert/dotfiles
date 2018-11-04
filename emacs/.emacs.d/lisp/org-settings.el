
(require 'use-package)

(use-package org
  :defer
  ;;:ensure-system-package ("sqlite3" "ledger" "gcc" "make" "mit-scheme")
  :config
  (add-to-list 'org-modules '(org-checklist
                              org-drill
                              org-jsinfo
                              org-git-link
                              ;; included by default, but critical
                              org-gnus))
  :bind (;; capture task to inbox
         ([f6]   . org-capture)
         ;; inbox, anything scheduled can be seen w/ f8
         ([f7]   . org-todo-list)
         ("C-c C-/" . org-toggle-timestamp-type)
         ("C-\\"    . smex)
         ([f8]   . org-agenda)
         ([C-f8] . org-agenda-kill-all-agenda-buffers))
  :config

  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)

  (use-package ob-http  :ensure t)

  ;; add :async to virtually any babel language
  (use-package ob-async :ensure t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (sqlite . t)
     (http . t)
     (sql . t)
     (makefile . t)
     (scheme . t)
     (C . t)
     (gnuplot . t)
     (ledger . t)
     (shell . t)))

  :custom
  (org-completion-use-ido t)
  (org-log-done 'time)
  ;; inbox.org captures all incoming tasks
  (org-default-notes-file "~/org/inbox.org")
  (org-agenda-text-search-extra-files '(agenda-archives))
  :init
  (setq
   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :maxlevel . 3))
   org-hide-leading-stars nil
   org-log-reschedule 'time
   org-log-into-drawer 'time
   org-src-fontify-natively t
   org-src-strip-leading-and-trailing-blank-lines t
   org-confirm-babel-evaluate nil))


(use-package org-agenda
  :after (org)
  :hook
  (( org-capture-after-finalize . org-save-all-org-buffers ))
  (( org-capture-prepare-finalize . org-save-all-org-buffers ))
  (( org-agenda-after-show . xref-pulse-momentarily ))
  :bind (:map org-agenda-mode-map
              ("C-c ="   . org-agenda-priority-up)
              ("C-c -"   . org-agenda-priority-down)
              ("j"   . org-agenda-next-item)
              ("k"   . org-agenda-previous-item))
  :init
  (use-package xref :demand)
  (setq org-agenda-todo-ignore-scheduled 'future
        org-agenda-window-setup 'current-window
        org-agenda-files '("~/org/")
        org-agenda-diary-file "~/org/journal.org"
        org-deadline-warning-days 7
        org-agenda-include-diary t
        org-log-done 'time)
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))
  :config
  (add-to-list 'org-agenda-custom-commands
               '("j" todo "TODO" ((org-agenda-max-entries 5))))
  (add-to-list 'org-agenda-custom-commands
               '("r" "inbox" tags "CATEGORY=\"inbox\"&LEVEL=2")))



(provide 'org-settings)
