
(use-package org
  :defer
  ;;:ensure-system-package ("sqlite3" "ledger" "gcc" "make" "mit-scheme")
  :config
  (add-to-list 'org-modules '(org-checklist org-drill org-jsinfo org-git-link org-gnus))
  :bind (;; capture task to inbox
         ([f6]   . org-capture)
         ;; inbox, anything scheduled can be seen w/ f8
         ([f7]   . org-todo-list)
         ("C-c C-/" . org-toggle-timestamp-type)
         ("C-\\"    . smex)
         ([f8]   . org-agenda)
         ([C-f8] . org-agenda-kill-all-agenda-buffers))
  ;; todo
  ;;:hook
  ;;;; Highlight the result of source block
  ;;((org-babel-after-execute .
  ;;                          (lambda ()
  ;;                            (when (eq this-command 'org-ctrl-c-ctrl-c)
  ;;                              (creichert/org-babel-highlight-result)))))
  ;;:preface
  ;;(defun creichert/org-babel-highlight-result ()
  ;;  "Highlight the result of the current source block. Adapt from `org-babel-remove-result'."
  ;;  (interactive)
  ;;  (let ((location (org-babel-where-is-src-block-result nil nil)))
  ;;    (when location
  ;;      (save-excursion
  ;;        (goto-char location)
  ;;        (when (looking-at (concat org-babel-result-regexp ".*$"))
  ;;          (pulse-momentary-highlight-region
  ;;           (1+ (match-end 0))
  ;;           (progn (forward-line 1) (org-babel-result-end))))))))
  :config
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (use-package ob-http  :ensure t)
  ;; add the :async keyword to any src block
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
     (ledger . t)
     (shell . t)))
  :custom
  (org-completion-use-ido t)
  (org-log-done 'time)
  ;;(org-log-done 'note)
  ;; ALL incoming org captures go into my inbox.
  (org-default-notes-file "~/org/inbox.org")
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


;;support spotify uri?
;;[[spotify:track:4cI0B5thREJ9g0RYpGVrhY][timebomb, Mr. 3-2 :: Str8 Drop]]
(use-package org-agenda
  :after (org)
  :hook
  (( org-capture-after-finalize . org-save-all-org-buffers ))
  (( org-capture-prepare-finalize . org-save-all-org-buffers ))
  :bind (:map org-agenda-mode-map
              ("C-c ="   . org-agenda-priority-up)
              ("C-c -"   . org-agenda-priority-down)
              ("j"   . org-agenda-next-item)
              ("k"   . org-agenda-previous-item))
  :init
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


;;; (defun org-todo-score (&optional ignore)
;;;   "Compute the score of an Org-mode task.
;;; Age gradually decreases the value given to a task.  After 28
;;; days, its score is zero.
;;; Effort should act as a multiplier on the value."
;;;   1)
;;;
;;; (defvar org-categories-pending-hashmap nil)
;;; (defvar org-categories-completed-hashmap nil)
;;;
;;; (defun org-compute-category-totals ()
;;;   (interactive)
;;;   (setq org-categories-pending-hashmap (make-hash-table :test 'equal)
;;;         org-categories-completed-hashmap (make-hash-table :test 'equal))
;;;   (dolist (file '("todo.txt" "archive.txt"))
;;;     (with-current-buffer
;;;         (find-file-noselect (expand-file-name file "~/Documents"))
;;;       (save-excursion
;;;         (goto-char (point-min))
;;;         (while (not (eobp))
;;;           (outline-next-heading)
;;;           (let* ((state (org-get-todo-state))
;;;                  (category
;;;                   (or (org-entry-get (point) "ARCHIVE_CATEGORY" t)
;;;                       (org-entry-get (point) "CATEGORY" t)))
;;;                  (hashmap
;;;                   (cond
;;;                    ((string= state "TODO") org-categories-pending-hashmap)
;;;                    ((string= state "DONE") org-categories-completed-hashmap)))
;;;                  (value (and hashmap (gethash category hashmap 0))))
;;;             (if hashmap
;;;                 (puthash category (+ value (org-todo-score)) hashmap))))))))
;;;
;;; (defun org-category-total (category)
;;;   ;; A category's final score is the sum of all open tasks (which raises the
;;;   ;; value), subtracted by the sum of all closed tasks.  Thus, a category with
;;;   ;; a higher score deserves more attention (it has been neglected or has not
;;;   ;; seen much activity), while a category with a low score deserves less.
;;;   ;;
;;;   ;; Note that this score is affected by several heuristics.  See
;;;   ;; `org-todo-score'.
;;;   (unless org-categories-pending-hashmap
;;;     (org-compute-category-totals))
;;;   (- (gethash category org-categories-pending-hashmap 0)
;;;      (gethash category org-categories-completed-hashmap 0)))
;;;
;;; (defun org-cmp-category-totals (a b)
;;;   (let ((cat-a (get-text-property 1 'org-category a))
;;;         (cat-b (get-text-property 1 'org-category b)))
;;;     (if (> (org-category-total cat-a)
;;;            (org-category-total cat-b))
;;;         1
;;;       -1)))

;; (setq org-agenda-cmp-user-defined 'org-cmp-category-totals)

(provide 'org-settings)
