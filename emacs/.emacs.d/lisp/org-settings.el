
(require 'use-package)

(use-package org
  :defer
  ;;:ensure-system-package ("sqlite3" "ledger" "gcc" "make" "mit-scheme")
  :bind (;; capture task to inbox
         ([f6]   . org-capture)
         ([f7]   . org-todo-list)
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
  (org-startup-with-inline-images t)
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
  :hook
  (( org-agenda-mode            . turn-on-auto-revert-mode ))
  (( org-capture-after-finalize . org-save-all-org-buffers ))
  (( org-capture-prepare-finalize . org-save-all-org-buffers ))
  (( org-agenda-after-show . xref-pulse-momentarily ))
  :bind (:map org-agenda-mode-map
              ("C-c ="   . org-agenda-priority-up)
              ("C-c -"   . org-agenda-priority-down)
              ("j"       . org-agenda-next-item)
              ("k"       . org-agenda-previous-item)
              ("C-\\"    . smex))
  :init
  (use-package xref :demand)
  (setq org-agenda-todo-ignore-scheduled 'future
        org-agenda-show-future-repeats nil
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
  ;; (setq org-stuck-projects
  ;;       '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "CATEGORY=\"cal\""))

  ;; Project-based commands

  (add-to-list 'org-agenda-custom-commands
               '("k"
                 "SimplyRETS"
                 ((agenda "" ((org-agenda-span 'day)))
                  (tags-todo "CATEGORY=\"simplyrets\"" ((org-agenda-max-entries 10))))
                 ((org-agenda-category-filter-preset '("+simplyrets")))))

  (add-to-list 'org-agenda-custom-commands
               '("l"
                 "Identibyte"
                 ((agenda "" ((org-agenda-span 'day)))
                  (tags-todo "CATEGORY=\"identibyte\"" ((org-agenda-max-entries 10))))
                 ((org-agenda-category-filter-preset '("+identibyte")))))


  (add-to-list 'org-agenda-custom-commands
               '("j"
                 "Assertible"
                 ((agenda "" ((org-agenda-span 'day)))
                  (tags-todo "CATEGORY=\"assertible\""
                             ((org-agenda-max-entries 10)
                              ;;(org-agenda-view-columns-initially t)
                              ;;(eval (setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TIMESTAMP"))
                              )))
                 ((org-agenda-category-filter-preset '("+assertible")))))

  ;; Inbox only
  (add-to-list 'org-agenda-custom-commands
               '("h" "Inbox" tags "CATEGORY=\"inbox\"&LEVEL=2"))

  ;; This should be the next 5 most important tasks
  (add-to-list 'org-agenda-custom-commands
               '("g" todo "Priority tasks (next)" ((org-agenda-max-entries 5))))

  ;; This should be a combined agent w/ Today's Agenda + Inbox
  (add-to-list 'org-agenda-custom-commands
               '("d"
                 "Today + Inbox"
                 ((agenda "" ((org-agenda-span 'day)))
                  (tags-todo "CATEGORY=\"inbox\"&LEVEL=2"
                             ))
                 ))
  )



(use-package org-checklist
  :ensure org-plus-contrib)

(use-package org-depend
  :ensure org-plus-contrib)

(use-package org-tempo
  :ensure org-plus-contrib)

(use-package org-gcal
  :ensure t
  :commands (org-gcal-fetch org-gcal-sync)
  :defer
  :config
  (use-package auth-source-pass :ensure t :demand)
  (setq org-gcal-client-id (auth-source-pass-get "user" "developers.google.com/org-gcal")
        org-gcal-file-alist '(("creichert07@gmail.com" . "~/org/cal.org"))
        org-gcal-client-secret (auth-source-pass-get 'secret "developers.google.com/org-gcal")))

(provide 'org-settings)
