
;;; creichert emacs configuration

;;; Code:

;; bootstrap use-package

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package bind-key                          :ensure t :demand)
(use-package use-package-ensure-system-package :ensure t :demand)
;; (setq use-package-expand-minimally t)
(setq use-package-compute-statistics t)

;;; configure emacs


;; Ensure system executables are installed for certain packages.
(setq source-directory "~/dev/c/emacs")
(setq custom-file "~/.emacs.d/custom.el")
(setq inhibit-startup-screen t)
(setq initial-scratch-message (format ";; startup took %s\n\n" (emacs-init-time)))
;; dont use any "gui" dialog boxes
(setq use-dialog-box nil)
;; make gui look like terminal
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use spaces instead of tabs, unless a mode/lang explicitly requires tabs.
(setq-default c-basic-offset 4)
(setq-default tab-always-indent t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

;; Write backup and auto-save files to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defalias 'yes-or-no-p 'y-or-n-p)


(defun reload-dotemacs ()
  "Reload init file without restarting Emacs."
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c :")       'reload-dotemacs)
(global-unset-key (kbd "C-z"))      ;; don't allow freeze
(global-unset-key (kbd "C-x C-z"))  ;; don't allow freeze


(use-package delsel
  :config (delete-selection-mode 1))


(use-package paren
  :hook ((prog-mode . show-paren-mode)))


(use-package help
  :custom
  (help-window-select t))


(use-package align
 :bind ("C-x /" . align-regexp))

(use-package replace
 :bind ("C-x C-/" . replace-regexp))


(use-package simple
  :bind
  ("C-c C-c M-x" . execute-extended-command)
  ("C-c E"       . first-error)
  ("C-c e"       . next-error)
  ("C-c C-e p"   . previous-error)
  :custom

  ;; clipboard
  ;;
  ;; share clipboard across the entire system
  (yank-pop-change-selection t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  :config
  ;;(transient-mark-mode 1)
  :init
  (setq
   mail-user-agent  'gnus-user-agent
   read-mail-command 'gnus))


(use-package savehist
  :custom
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :init
  (setq savehist-additional-variables
        '( ;;kill-ring  adds photos & other very large files
          compile-command
          search-ring
          regexp-search-ring))
  :config
  (savehist-mode 1))


(use-package vc
  :defer
  :custom
  (vc-follow-symlinks t))


;;(use-package xref
;;  :config
;;  (add-to-list 'xref-after-jump-hook '(delete-windows-on "*xref*")
;;  :init
;;  (setq xref-show-xrefs-function
;;        (lambda (xrefs alist)
;;          (let ((buffer (xref--show-xref-buffer xrefs alist)))
;;            (quit-window)
;;            (let ((orig-buf (current-buffer))
;;                  (orig-pos (point))
;;                  (done)
;;                  (candidate
;;                   ;;(ido-completing-read+
;;                   (completing-read
;;                    "xref: "
;;                    (let ((collection nil))
;;                      (dolist (xref xrefs)
;;                        (with-slots (summary location) xref
;;                          (let* ((line (xref-location-line location))
;;                                 (file (xref-location-group location))
;;                                 (candidate
;;                                  (concat
;;                                   (propertize
;;                                    (concat
;;                                     file
;;                                     ;;(if ivy-xref-use-file-path
;;                                     ;;    file
;;                                     ;;  (file-name-nondirectory file))
;;                                     (if (integerp line)
;;                                         (format ":%d: " line)
;;                                       ": "))
;;                                    'face 'compilation-info)
;;                                   (progn
;;                                     ;;(when ivy-xref-remove-text-properties
;;                                     (set-text-properties 0 (length summary) nil summary)
;;                                     ;;)
;;                                     summary
;;                                     ))))
;;                            ;;xref-etags-location
;;                            (push `(,candidate . ,location) collection))))
;;                            ;;(push (cons candidate xref) collection))))
;;                      (nreverse collection))))
;;                   )
;;               ;;(setq done (eq 'ivy-done this-command))
;;               (condition-case err
;;                   ;; (let* ((marker (xref-location-marker ((car candidate) (cdr candidate))))
;;                   ;; (let* ((marker (xref-location-marker (cdr candidate)))
;;                   (with-slots (summary location) candidate
;;                   (let* ((marker (xref-location-marker location))
;;                          (buf (marker-buffer marker)))
;;                     (with-current-buffer buffer
;;                       (select-window
;;                        ;; function signature changed in
;;                        ;; 2a973edeacefcabb9fd8024188b7e167f0f9a9b6
;;                        (if (version< emacs-version "26.0.90")
;;                            (xref--show-pos-in-buf marker buf t)
;;                          (xref--show-pos-in-buf marker buf)))))
;;                   )
;;                 (user-error (message (error-message-string err)))))
;;
;;            buffer)
;;        ))
;;)


(use-package ido
  :demand
  :bind
  (("C-x f" . ido-find-file)
   ("C-c C-x C-o" . ido-switch-buffer-other-window))
  :config
  ;; (add-to-list 'ido-ignore-files "\\.rej$")
  ;; (add-to-list 'ido-ignore-files "\\.dyn_hi$")
  ;; (add-to-list 'ido-ignore-files "\\.dyn_o$")
  ;; (add-to-list 'ido-ignore-files "\\.hi$")
  ;; (add-to-list 'ido-ignore-files "\\.o$")
  (add-to-list 'ido-ignore-files "\\.tags$")
  ;; (add-to-list 'ido-ignore-files "\\TAGS$")
  ;; (add-to-list 'ido-ignore-buffers "*Compile-Log*")
  ;; (add-to-list 'ido-ignore-buffers "*Help*")
  ;; (add-to-list 'ido-ignore-buffers "TAGS")
  (ido-mode 1)
  (ido-everywhere 1)
  :init
  (setq ido-max-directory-size 100000
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-faces t
        ido-use-filename-at-point 'nil))


(use-package ido-vertical-mode
  :load-path "site-lisp/ido-vertical-mode.el/"
  :requires (ido)
  :config (ido-vertical-mode)
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))


(use-package flx-ido
  :ensure t
  :requires (ido)
  :config
  (flx-ido-mode)
  :init
  (setq flx-ido-use-faces t)
  (setq flx-ido-threshold 1000))


(use-package ido-at-point
  :ensure t
  :requires (ido)
  :config
  (ido-at-point-mode))


(use-package ido-completing-read+
  :ensure t
  :requires (ido)
  :config
  (ido-ubiquitous-mode t)
  :init
  (setq completing-read-function 'ido-completing-read+
        ido-cr+-max-items nil))


(use-package projectile
  :ensure t
  :requires (ido)
  ;;:hook (( projectile-after-switch-project . magit-status ))
  :bind
  (("C-x C-d" . projectile-switch-project)
   (:map projectile-mode-map
         ("C-x C-f" . projectile-find-file)
         ([f5]      . projectile-compile-project)
         ;;("f"       . projectile-find-tag))
         ("C-c g"   . projectile-grep)))
  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        ;;projectile-sort-order 'modification-time
        projectile-use-git-grep 't
        ;;projectile-project-search-path '("~/dev")
        projectile-globally-ignored-directories '("~/.stack/snapshots")
        projectile-tags-command "make tags"
        )
  :config
  (projectile-mode)
  (projectile-register-project-type
   'haskell-makefile '("stack.yaml" "Makefile")
   :compile "make"
   :test "make test"
   :run "make dev"
   :test-suffix "Spec"))


(use-package smex
  :commands (smex smex-major-mode-commands)
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))


;; start in a state that immediately supports typing or direct emacs keybindings
(use-package etags
  :defer
  :custom
  (tags-revert-without-query t)
  :config
  (setq tags-add-tables nil))


;; Xresource based styles
;;
;; Uses colors supplied through xresources (xrdb -query) to make emacs
;; consistent with other desktop applications. In theory, all apps should use
;; this but "larger" apps like Chrome & Gnome apps will often just ignore it.
;; This is only needed once, near the top of the file
(use-package xresources-theme
  :ensure t
  :if window-system ;; display-graphic-p
  :load-path "themes"
  :config
  ;; NOT WORKING
  (setq ansi-color-names-vector (vector
                                 (xresources-theme-color "background")
                                 (xresources-theme-color "color1")
                                 (xresources-theme-color "color2")
                                 (xresources-theme-color "color3")
                                 (xresources-theme-color "color4")
                                 (xresources-theme-color "color5")
                                 (xresources-theme-color "color6")
                                 (xresources-theme-color "foreground")
                                 ))
  (load-theme 'xresources t))


(use-package ansi-color
  :hook
  (( compilation-filter . (lambda ()
                            (let ((inhibit-read-only t))
                              (ansi-color-apply-on-region (point-min) (point-max)))) )))


(use-package compile
  :defer
  ;;:hook ((compilation-mode . (lambda () (setq scroll-margin 0))
  :init
  (setq compilation-read-command nil
        compilation-scroll-output t
        ;; make compilation-mode a lot faster but excluding cpu intensive regexp's which
        ;; clog up the buffer on long lines.
       compilation-error-regexp-alist
         '(absoft bash borland msft epc ibm java gcc-include gnu lcc
           mips-1 mips-2 omake oracle perl php sun sun-ada watcom
           4bsd gcov-file gcov-header gcov-nomark gcov-called-line
           gcov-never-called weblint guile-file guile-line
           ;; disabled compilation regexps
           ;;
           ;; ada aix ant python-tracebacks-and-caml comma cucumber edg-1 edg-2
           ;; ftnchek iar irix jikes-file maven jikes-line ruby-Test::Unit makepp
           ;; rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example
           ;; perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness
          ))

  :config

  (setq scroll-margin 0
        scroll-step 1
        scroll-conservatively 10000)

  (defvar compilation-buffer-visible nil)

  (defun toggle-compilation-visible ()
    (interactive)
    (setq compilation-buffer-visible (not compilation-buffer-visible))
    (message "Compilation buffer %s"
             (if compilation-buffer-visible "visible" "not visible")))

  (defun notify-compilation-result(buffer msg)
    (with-current-buffer buffer
      (progn
        (cond
         ((and (string-match "^finished" msg) (string= "*compilation*" (buffer-name)))
          (progn
            (unless compilation-buffer-visible (delete-windows-on buffer))))
         ((string= "*compilation*" (buffer-name))
          (progn
            ;; nothing todo
            )))
        (setq current-frame (car (car (cdr (current-frame-configuration)))))
        (raise-frame current-frame))))

  (add-to-list 'compilation-finish-functions 'notify-compilation-result))


(use-package whitespace
  :defer
  :preface
  (defun whitespace-local-mode ()
    (add-hook (make-local-variable 'before-save-hook)
              'delete-trailing-whitespace))
  :hook ((prog-mode . whitespace-local-mode)))


(use-package register
  :defer
  :bind
  (([f10] . (lambda ()
              (interactive)
              (jump-to-register 9)
              ;; make sure to put compilation-buffer at end here.
              (message "Windows disposition loaded"))))
  (([C-f10] . (lambda ()
                (interactive)
                (window-configuration-to-register 9)
                (message "Windows disposition saved")))))


(use-package evil
  :ensure t
  :hook
  ((with-editor-mode . evil-insert-state))
  ((with-presentation-mode . evil-motion-state))
  ((archive-mode . evil-motion-state))
  ((prog-mode . (lambda ()
                  (progn
                    (defalias #'forward-evil-word #'forward-evil-symbol)))))
  :config
  ;;((special-mode . evil-emacs-state))
  ;;((xref--show-xref-buffer-mode . evil-emacs-state))
  ;;((prog-mode . (lambda ()
  ;;                (defalias #'forward-evil-word #'forward-evil-symbol))
  ;;
  ;; vim-like bindings in the minibuffer
  ;;
  ;; doing it this way is a little tricky:
  ;;
  ;; (setq evil-want-minibuffer t)
  ;;
  ;; Instead, these keybinding give just enough power to scroll around
  ;; and make edits in the minibuffer quickly:
  ;;
  ;;   - C-w : delete backward word
  ;;   - C-k : prev history
  ;;   - C-j : next history
  ;;
  :bind (:map evil-motion-state-map
              ("f" . xref-find-definitions)
              ("s" . xref-pop-marker-stack)
              :map evil-normal-state-map
              ("f" . xref-find-definitions)
              ("s" . xref-pop-marker-stack)
              (";" . evil-ex)
              ("\\" . smex)
              :map evil-insert-state-map
              ("C-\\" . smex)
              ("j" . evil-maybe-exit)
              :map minibuffer-local-map
              ("C-j" . next-history-element)
              ("C-k" . previous-history-element)
              ("C-w" . evil-delete-backward-word)
              :map minibuffer-inactive-mode-map
              ("C-j" . next-history-element)
              ("C-k" . previous-history-element)
              :map minibuffer-local-ns-map
              ("C-j" . next-history-element)
              ("C-k" . previous-history-element)
              :map minibuffer-local-isearch-map
              ("C-j" . next-history-element)
              ("C-k" . previous-history-element)
              :map minibuffer-local-completion-map
              ("C-j" . next-history-element)
              ("C-k" . previous-history-element)
              :map minibuffer-local-must-match-map
              ("C-j" . next-history-element)
              ("C-k" . previous-history-element))
  :init
  (setq evil-default-state 'normal)
  ;; :config not working w/ most of config. evil is simply loaded immediately
  ;; instead of lazily
  (use-package evil-leader
    :ensure t
    :init
    (setq evil-leader/leader "SPC"
          evil-leader/in-all-states t
          evil-leader/non-normal-prefix "C-"
          evil-leader/no-prefix-mode-rx
          '("magit-.*-mode"
            "*Messages*"
            "gnus-.*-mode"))
    (global-evil-leader-mode 1))
  (evil-mode 1)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; exit insert mode if I lean on 'j' button
  (evil-define-command evil-maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                             nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?j))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))))))
  (defun find-def ()
    (interactive)
    (cond
     ((fboundp 'projectile-find-tag) (projectile-find-tag))
     ((fboundp 'ggtags-find-definitions) (ggtags-find-definitions))
     (t (xref-find-definitions))))
  (defun recompile-quietly ()
    "Re-compile without changing the window configuration."
    (interactive)
    (save-window-excursion
      (projectile-compile-project nil)))

  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  (evil-leader/set-key
    "1"       'delete-other-windows
    "o"       'other-window
    "xc"      'save-buffers-kill-terminal

    "F"       'find-def
    "S"       'xref-pop-marker-stack

    "tc"      'toggle-compilation-visible
    "v"       'recompile-quietly ;; still under projectile context
    "k"       'recompile-quietly ;; still under projectile context

    "f"       'projectile-find-file
    "prt"     'projectile-regenerate-tags
    "c"       'projectile-compile-project
    "b"       'projectile-switch-to-buffer
    "ptt"     'projectile-toggle-between-implementation-and-test
    "e"       'next-error
    "w"       'previous-error
    "E"       'flycheck-next-error
    "W"       'flycheck-previous-error
    ")"       'evil-next-close-paren
    "("       'insert-parentheses
    "9"       'insert-parentheses

    ;; currently overlapping to see which i prefer
    "gpgr"    'epa-sign-region
    "gpgf"    'epa-sign-file
    "gpgvr"   'epa-verify-region
    "gpgvf"   'epa-verify-file

    "isw"     'ispell-word

    "u"       'browse-url
    "U"       'browse-url-chromium
    "G"       'google-this
    ))


(use-package w3m
 :ensure t
 :commands (w3m-browse-url w3m-find-file)
 ;;:ensure-system-package ("w3m")
 :init (setq
        browse-url-browser-function
        '(("github.com" . browse-url-chromium)
          ("trello.com" . browse-url-chromium)
          ("circleci.com" . browse-url-chromium)
          ("pagerduty.com" . browse-url-chromium)
          ("accounts.google.com" . browse-url-chromium)
          ("accounts.spotify.com" . browse-url-chromium)
          ("assertible.com" . browse-url-chromium)
          ("simplyrets.com/admin" . browse-url-chromium)
          ("slack.com" . browse-url-chromium)
          ("rollbar.com" . browse-url-chromium)
          ("app.drift.com" . browse-url-chromium)
          ("gmail.com" . browse-url-chromium)
          ("aws.amazon.com" . browse-url-chromium)
          ("youtube.com" . browse-url-chromium)
          ("facebook.com" . browse-url-chromium)
          ("upwork.com" . browse-url-chromium)
          (".*\\.gov" . browse-url-chromium)
          ("docusign.com\\|docusign.net" . browse-url-chromium)
          ("." . w3m-browse-url))))


(use-package magit
  :bind (([f9]   . magit-status)
         ([C-f9] . magit-log))
  :config (setq magit-completing-read-function 'magit-ido-completing-read)
  :requires (evil)
  :ensure t
  :config
  (evil-set-initial-state 'magit-log-edit-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-diff-mode-map 'emacs)
  ;; (evil-set-initial-state 'special-mode 'emacs)
  (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
    "K" 'magit-discard
    "L" 'magit-key-mode-popup-logging)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    "K" 'magit-discard
    "l" 'magit-key-mode-popup-logging
    "h" 'magit-toggle-diff-refine-hunk)
  (evil-define-key 'motion magit-status-mode-map
    "\C-f" 'evil-scroll-page-down
    "\C-b" 'evil-scroll-page-up
    "." 'magit-mark-item
    "=" 'magit-diff-with-mark
    "C" 'magit-add-log
    "I" 'magit-ignore-item-locally
    "S" 'magit-stage-all
    "U" 'magit-unstage-all
    "X" 'magit-reset-working-tree
    "i" 'magit-ignore-item
    "s" 'magit-stage-item
    "u" 'magit-unstage-item
    "K" 'magit-discard
    "z" 'magit-key-mode-popup-stashing)

  (evil-leader/set-key-for-mode 'magit-status-mode
    "SPC" 'magit-stash-show))


(use-package evil-magit
  :ensure t
  :requires (magit evil)

  :bind (:map evil-normal-state-map
              ("\\" . smex)
              :map evil-insert-state-map
              ("C-\\" . smex)
              :map magit-status-mode-map
              ("\\" . smex)
              ("C-\\" . smex))

  :config
  (evil-define-key evil-magit-state magit-mode-map
    "p" 'magit-section-backward
    "n" 'magit-section-forward))


(use-package flycheck
  :ensure t
  :hook ((after-init . global-flycheck-mode))
  ;;:ensure-system-package ((proselint . "pip install proselint"))
  :config
  (setq flycheck-standard-error-navigation nil)
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    ;; doesn't work well with org-mode
    :modes (message-mode text-mode markdown-mode gfm-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)))


(use-package flyspell
  :requires (flycheck)
  :hook ((markdown-mode . turn-on-flyspell))
        ((prog-mode     . flyspell-prog-mode)))



(use-package sql
  :defer
  ;;:ensure-system-package ("postgresql-client-common")
  :init
  :config
  (add-to-list 'sql-postgres-options "--no-psqlrc"))


(use-package sql-pgpass
  :after (sql)
  :load-path "lisp/")


(use-package sh-script
  :mode
  ("\\.bash_functions.local\\'" . sh-mode)
  ("\\.bash_functions\\'"      . sh-mode)
  ("\\.bash_aliases\\'"        . sh-mode))


(use-package js
  :mode "\\.json.template\\'")


(use-package conf-mode
  :mode "\\.inputrc\\'")


(use-package markdown-mode
  :ensure t
  ;;:ensure-system-package ("markdown")
  :mode ("\\.markdown\\'" . markdown-mode))


(use-package dotenv-mode
  :ensure t
  :mode ("\\.env.sample" . dotenv-mode))


(use-package yaml-mode
  ;;:ensure-system-package yamllint
  :ensure t :defer)


(use-package flycheck-yamllint
  ;;:ensure-system-package yamllint
  :ensure t :defer)


(use-package google-this
  ;;:ensure-system-package ("chromium")
  :ensure t :defer)


;; Gnus: required .emacs settings
(use-package gnus
  :commands gnus
  :custom
  (gnus-home-directory "~/")
  (gnus-directory "~/.emacs.d/gnus/news/")
  (message-directory "~/.emacs.d/gnus/mail/")
  (nnfolder-directory "~/.emacs.d/gnus/mail/"))


(use-package mm-decode
  :custom
   (mm-coding-system-priorities '(utf-8 iso-latin-1 iso-latin-9 mule-utf-8))
   (mm-verify-option 'always)
   (mm-decrypt-option 'always))


(use-package epa
  :defer
  ;;:ensure-system-package (gpg2 . gnupg2)
  :custom
  (epa-pinentry-mode 'loopback))

;; minimal modeline
;;
;; (set-face-attribute 'mode-line-emphasis :weight 1)
;; (set-face-attribute 'mode-line-highlight :background (x-get-resource "color2" ""))
;; (mode-line ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-silver))))
;; (mode-line-buffer-id ((t (:weight bold))))
;; (mode-line-inactive ((t (:background ,atom-one-dark-gray))))
(use-package faces
  :config
  (set-face-attribute 'mode-line nil :box '(:width 0.5))
  (set-face-attribute 'mode-line-inactive nil :box nil))


(use-package org-settings
  :load-path "lisp/")


(use-package bbdb-settings
  :load-path "lisp/")


(use-package haskell-settings
  :load-path "lisp/")


(use-package web-settings
  :load-path "lisp/")


;; extra emacs packages & utilities I use which aren't "core"
(use-package extra
  :load-path "lisp"
  :if (file-exists-p "~/.emacs.d/lisp/extra.el"))
