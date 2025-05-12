
(require 'use-package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa"))

(use-package org
  :defer
  ;:ensure-system-package ("sqlite3" "gcc" "make" "mit-scheme")
  :hook ((org-mode . visual-line-mode))
  :bind (([f6]   . org-capture)
         ([f7]   . org-todo-list)
         ([f8]   . org-agenda)
         ([C-f8] . org-save-all-org-buffers))
  :config

  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

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
     (shell . t)))

  (setq
   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :maxlevel . 3))
   org-hide-leading-stars nil
   org-log-reschedule 'time
   org-log-into-drawer 'time
   org-src-fontify-natively t
   org-src-strip-leading-and-trailing-blank-lines t
   org-confirm-babel-evaluate nil)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" ; doing later
          ;"NEXT(n)" ; doing now or soon
           "|"
           "DONE(d)" ; done
           )
          ;(sequence
          ; "WAIT(w)" ; waiting for some external change (event)
          ; "HOLD(h)" ; waiting for some internal change (of mind)
          ; "IDEA(i)" ; maybe someday
          ; "|"
          ; "NOTE(o@/!)" ; end state, just keep track of it
          ; "STOP(s@/!)" ; stopped waiting, decided not to work on it
          ; )
          ))

  :custom
  (org-startup-with-inline-images t)
  (org-completion-use-ido t)
  (org-log-done 'time)
  (org-directory "~/dev/org")
  ;; inbox.org captures all incoming tasks
  (org-default-notes-file "~/dev/org/inbox.org")
  (org-agenda-text-search-extra-files '(agenda-archives))
  ;; allow ido completion
  (org-outline-path-complete-in-steps nil)
  )


(use-package org-agenda
  :after (org)
  :hook
  (( org-agenda-mode              . turn-on-auto-revert-mode ))
  (( org-capture-after-finalize   . org-save-all-org-buffers ))
  (( org-capture-prepare-finalize . org-save-all-org-buffers ))
  (( org-agenda-after-show        . xref-pulse-momentarily ))
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
        org-agenda-files '("~/dev/org/" "~/dev/org/roam/daily/")
        org-agenda-diary-file "~/dev/org/journal.org"
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
               '("r"
                 "rbros tasks/projects"
                 ((agenda "" ((org-agenda-span 'day)))
                  (tags-todo "CATEGORY=\"rbros\"" ((org-agenda-max-entries 10))))
                 ((org-agenda-category-filter-preset '("+rbros")))))
  (add-to-list 'org-agenda-custom-commands
               '("p"
                 "personal tasks/projects"
                 ((agenda "" ((org-agenda-span 'day)))
                  (tags-todo "CATEGORY=\"me\"" ((org-agenda-max-entries 10))))
                 ((org-agenda-category-filter-preset '("+me")))))

  ;; Inbox only
  (add-to-list 'org-agenda-custom-commands
               '("h" "Inbox" tags "CATEGORY=\"inbox\"&LEVEL=2"))

  ;; This should be the next 5 most important tasks
  (add-to-list 'org-agenda-custom-commands
               '("g" alltodo "Priority tasks (next)" ((org-agenda-max-entries 5))))

  ;; This should be a combined agent w/ Today's Agenda + Inbox
  (add-to-list 'org-agenda-custom-commands
               '("d"
                 "Today + Inbox"
                 ((agenda "" ((org-agenda-span 'day)))
                  ; in the inbox
                  (tags-todo "CATEGORY=\"inbox\"&LEVEL=2")
                  ;; in the inbox or categorized unschedule
                  (tags-todo "-CATEGORY=\"inbox\"&-SCHEDULED={.+}&LEVEL=1")
                  )))
  )


(use-package org-roam
  :ensure t
  :after (org)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/dev/org/roam")
  (org-roam-completion-everywhere t)
  ;; Capture a journal entry
  ;;
  ;; Note that for daily files to show up in the calendar, they have to be of
  ;; format \"org-time-string.org\" which the below does not seem to be.
  (org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?\n  <%<%Y-%m-%d %a %H:%M>>\n"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+category: journal\n\n"))))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-buffer-refresh)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ;; create an org-roam node from heading
         ("C-c n c" . org-id-get-create)
         ;("C-c n s" . <TODO SEARCH>)
         ([C-f6] . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ; C-c n d <key>
         ;("n" . org-roam-dailies-capture-today)
         ;("d" . org-roam-dailies-goto-today)
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-roam-node-display-template
        (concat "${title:*}"
                (propertize "${tags:10}" 'face 'org-tag)))
  ;; allow space in minibuffer when creating nodes
  (define-key ido-common-completion-map (kbd "SPC") 'self-insert-command)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))



(provide 'org-settings)
