
;;; creichert emacs configuration

;;; Code:

;; bootstrap use-package

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-compute-statistics t)


;; minor modes
;;(use-package font-core
;;  :config (global-font-lock-mode 1))
;;
;;(use-package delsel
;;  :config (delete-selection-mode 1))
;;
;;(use-package jka-cmpr-hook
;;  :config (auto-compression-mode t))
;;
;;(use-package frame
;;  :config (blink-cursor-mode 0))
;;
;;(use-package linum
;;  :config (global-linum-mode -1))
(use-package paren
  :hook ((prog-mode . show-paren-mode)))

(setq custom-file "~/.emacs.d/custom.el")
(setq inhibit-startup-screen t)
(setq initial-scratch-message (format ";; startup took %s\n\n" (emacs-init-time)))

;; dont use any "gui" dialog boxes
(setq use-dialog-box nil)
(setq blink-cursor-delay 0)

;; make gui look like terminal
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; clipboard
;;
;; share clipboard across the entire system
(setq save-interprogram-paste-before-kill t)
(setq yank-pop-change-selection t)

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

(use-package align
 :bind ("C-x /" . align-regexp))

(use-package replace
 :bind ("C-x C-/" . replace-regexp))


(use-package simple
  :bind
  ("C-c C-c M-x" . execute-extended-command)
  ("C-c b"       . execute-extended-command)
  ("C-c E"       . first-error)
  ("C-c e"       . next-error)
  ("C-c C-e p"   . previous-error)
  :config
  ;;(transient-mark-mode 1)
  :init
  (setq
   mail-user-agent  'gnus-user-agent
   read-mail-command 'gnus))


;; history
;;
(use-package savehist
  :init
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables
        '(kill-ring
          compile-command
          search-ring
          regexp-search-ring))

  :config
  (savehist-mode 1))


(use-package ido
  :demand
  :bind
  ("C-x f" . ido-find-file)
  :config
  (add-to-list 'ido-ignore-files "\\.rej$")
  (add-to-list 'ido-ignore-files "\\.dyn_hi$")
  (add-to-list 'ido-ignore-files "\\.dyn_o$")
  (add-to-list 'ido-ignore-files "\\.hi$")
  (add-to-list 'ido-ignore-files "\\.o$")
  (add-to-list 'ido-ignore-files "\\.tags$")
  (add-to-list 'ido-ignore-files "\\TAGS$")
  (add-to-list 'ido-ignore-buffers "*Compile-Log*")
  (add-to-list 'ido-ignore-buffers "*Help*")
  (add-to-list 'ido-ignore-buffers "TAGS")
  (ido-mode 1)
  (ido-everywhere 1)
  :init
  (setq ido-max-directory-size 100000
        ido-enable-flex-matching t
        ido-max-prospects 5
        ido-create-new-buffer 'always
        ido-use-faces t
        ido-use-filename-at-point 'nil))


(use-package ido-vertical-mode
  :load-path "~/.emacs.d/ido-vertical-mode.el/"
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
  :init
  (setq completing-read-function 'ido-completing-read+
        ido-cr+-max-items nil)
  :config
  (ido-ubiquitous-mode t))


(use-package projectile
  :ensure t
  :requires (ido)

  :hook (( projectile-after-switch-project . magit-status ))
  :bind
  ("C-x C-f" . projectile-find-file)
  ("C-x C-d" . projectile-switch-project)
  ([f5]      . projectile-compile-project)
  ("C-c g"   . projectile-grep)

  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        ;;projectile-sort-order 'recently-active
        projectile-sort-order 'modification-time
        projectile-use-git-grep 't
        ;;projectile-project-search-path '("~/dev")
        projectile-globally-ignored-directories '("~/.stack/snapshots")
        projectile-tags-command "make tags")
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
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))


;; start in a state that immediately supports typing or direct emacs keybindings
(use-package etags
  :defer
  :init
  (setq tags-revert-without-query 1)
  ;;(setq tags-case-fold-search nil) ;; case-insensitive
  (setq xref-etags-mode t)
  )

(use-package etags-select
  :requires (etags)
  :load-path "~/.emacs.d/etags-select.el/")


;; Xresource based styles
;;
;; Uses colors supplied through xresources (xrdb -query) to make emacs
;; consistent with other desktop applications. In theory, all apps should use
;; this but "larger" apps like Chrome & Gnome apps will often just ignore it.
;; This is only needed once, near the top of the file
(use-package xresources-theme
  :ensure t
  :if window-system ;; display-graphic-p
  :config (load-theme 'xresources t)
  :load-path "themes")



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
            (unless compilation-buffer-visible (delete-windows-on buffer))
            ))
         ((string= "*compilation*" (buffer-name))
          (progn
            ))
         )
        (setq current-frame (car (car (cdr (current-frame-configuration)))))
        (raise-frame current-frame))))

  (add-to-list 'compilation-finish-functions 'notify-compilation-result))

(use-package whitespace
  :defer
  :hook ((prog-mode
          . (lambda ()
              (add-hook (make-local-variable 'before-save-hook)
                        'delete-trailing-whitespace)))))

(global-set-key [f10]
                '(lambda ()
                   (interactive)
                   (jump-to-register 9)
                   ;; make sure to put compilation-buffer at end here.
                   (message "Windows disposition loaded")))

(global-set-key [C-f10]
                '(lambda ()
                   (interactive)
           (window-configuration-to-register 9)
                   (message "Windows disposition saved")))


;; evil

(use-package evil
  :ensure t
  :hook
  ((with-editor-mode . evil-insert-state))
  ((with-presentation-mode . evil-insert-state))
  ((archive-mode-hook . evil-motion-state))

  :init
  (setq evil-default-state 'normal)
  ;; setup evil leader
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
    (global-evil-leader-mode 1)
    )
  (evil-mode 1)

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
              :map evil-normal-state-map
              ("s" . pop-tag-mark)
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
  :config
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


  (defun recompile-quietly ()
    "Re-compile without changing the window configuration."
    (interactive)
    (save-window-excursion
      (projectile-compile-project nil)))

  (evil-leader/set-key
    "o"       'other-window
    "xc"      'save-buffers-kill-terminal

    "F"       'xref-find-definitions
    "S"       'pop-tag-mark

    "tc"      'toggle-compilation-visible
    "v"       'recompile-quietly ;; still under projectile context
    "k"       'recompile-quietly ;; still under projectile context

    "f"       'projectile-find-file
    "prt"     'projectile-regenerate-tags
    "c"       'projectile-compile-project
    "b"       'projectile-switch-to-buffer
    "ptt"     'projectile-toggle-between-implementation-and-test

    "e"       'flycheck-next-error
    "w"       'flycheck-previous-error

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
 :init (setq
        browse-url-browser-function
        '(("github.com" . browse-url-chromium)
          ("accounts.google.com" . browse-url-chromium)
          ("assertible.com" . browse-url-chromium)
          ("slack.com" . browse-url-chromium)
          ("rollbar.com" . browse-url-chromium)
          ("app.drift.com" . browse-url-chromium)
          ("gmail.com" . browse-url-chromium)
          ("aws.amazon.com" . browse-url-chromium)
          ("youtube.com" . browse-url-chromium)
          ("facebook.com" . browse-url-chromium)
          ("docusign.com\\|docusign.net" . browse-url-chromium)
          ("." . w3m-browse-url))))


(use-package magit
  :bind (([f9]   . magit-status)
         ([C-f9] . magit-log))
  :config (setq magit-completing-read-function 'magit-ido-completing-read)
  :ensure t
  :init
  (evil-set-initial-state 'magit-log-edit-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-diff-mode-map 'emacs)
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
              ( "\\" . smex)
              :map evil-insert-state-map
              ( "C-\\" . smex)
              :map magit-status-mode-map
              ( "\\" . smex)
              ( "C-\\" . smex))

  :config
  (evil-define-key evil-magit-state magit-mode-map
    "p" 'magit-section-backward
    "n" 'magit-section-forward))


(use-package flycheck
  :ensure t
  :hook ((after-init . global-flycheck-mode))
  :init (setq flycheck-standard-error-navigation nil)
  )


(use-package flyspell
  :requires (flycheck)
  :hook ((prog-mode . flyspell-prog-mode)))


(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" "\\.lhs\\'")
  :interpreter
  ("stack"      . haskell-mode)
  ("runhaskell" . haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-;" . haskell-process-load-file)
              ("C-c C-l" . haskell-process-reload)
              ("C-c i"   . haskell-navigate-imports-go)
              ("C-c I"   . haskell-navigate-imports-return)
              ("C-c C-j" . haskell-run-function-under-cursor))

  :init
  (setq haskell-process-args-stack-ghci '("--ghci-options=-O0"))
  (setq haskell-stylish-on-save t

        ;;haskell-process-suggest-hoogle-imports t
        haskell-process-suggest-haskell-docs-imports t
        haskell-process-suggest-remove-import-lines t
        ;;haskell-process-suggest-restart nil

        ;;haskell-interactive-mode-eval-mode t
        ;; this is set automatically when there is a `stack.yaml`
        ;; haskell-process-type 'stack-ghci
        ;; print type info to presentation-mode instead
        ;; of message area.
        ;; haskell-process-use-presentation-mode t
        ;;
        ;; bytecode takes up more memory than object code.
        ;; enable
        ;; haskell-process-reload-with-fbytecode nil
        ;;
        ;; experimenting with brittany
        ;; haskell-mode-stylish-haskell-path "brittany"
        ;;
        ;; only needed if having issues
        ;; haskell-process-log t
        ;; haskell-font-lock-quasi-quote-modes
        ;; (append '(("yamlQQ" . yaml-mode) ("js" . js-mode)) haskell-font-lock-quasi-quote-modes)
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        ;;haskell-indentation-starter-offset 4
        )

  :hook ((haskell-mode . haskell-doc-mode))
        ((haskell-mode . haskell-collapse-mode))
        ((haskell-mode . haskell-decl-scan-mode))
        ((haskell-mode . haskell-indentation-mode))
        ((haskell-mode . electric-pair-local-mode))
        ((haskell-mode . electric-indent-local-mode))

  :config

  ;; https://gist.github.com/989ad8be92f68682abff
  (defun haskell-run-function-under-cursor ()
    "Send the word-at-point as a function to GHCi process."
    (interactive)
    ;; (haskell-process-set-sent-stdin 't)
    (haskell-process-send-string
     (haskell-session-process (haskell-session-maybe))
     (format "%s" (word-at-point))))

  (evil-leader/set-key-for-mode 'haskell-mode
    "hir" 'hindent-reformat-region
    "hid" 'hindent-reformat-decl-or-fill
    "f" 'haskell-mode-jump-to-def-or-tag
    "TAB" 'haskell-hide-toggle
    "l" 'haskell-process-load-or-reload)

  (setq electric-layout-rules '((?\{) (?\} . around))))


(use-package hindent
  :ensure t
  ;; :init (hindent-mode)
  :after (haskell-mode)
  :hook ((haskell-mode . hindent-mode)))

(use-package flycheck-haskell
  :ensure t
  :after (flycheck haskell-mode)
  :init (setq flycheck-ghc-args '("-Wall"))
  :hook ((haskell-mode . flycheck-haskell-setup)))


(use-package web-mode
  :ensure t
  :mode "\\.js\\'"
  :init (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
              ;; might have to set in web-mode-hook
              web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4)

  ;;:hook ((web-mode . emmet-mode))
  :config

  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)

  ;; https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/lunaryorn-flycheck.el#L62
  (defun use-eslint-from-node-modules ()
    "Find the eslint binary local to the current file to use the correct configuration, plugins, etc."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook 'use-eslint-from-node-modules)
  )

(use-package prettier-js
 :ensure t
 :hook ((web-mode . prettier-js-mode))
 :init
  (setq prettier-js-args '("--tab-width" "4"
                           "--trailing-comma" "es5"
                           "--print-width" "90"
                           "--jsx-bracket-same-line" "true"
                           "--no-semi"
                           )))


(use-package sql
  :defer
  :config
  (add-to-list sql-postgres-options '("--no-psqlrc")))


(use-package sh
  :mode
  ("\\.bash_functions.local\\'" . sh-mode)
  ("\\.bash_functions\\'"      . sh-mode)
  ("\\.bash_aliases\\'"        . sh-mode))


(use-package javascript-mode
  :mode "\\.json.template\\'")


(use-package conf-mode
  :mode "\\.inputrc\\'")


(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" . markdown-mode))


(use-package dotenv-mode
  :ensure t
  :mode ("\\.env.sample" . dotenv-mode))


(use-package dockerfile-mode
  :ensure t
  :mode ("\\.Dockerfile.\\'" . dockerfile-mode))


(use-package yaml-mode :ensure t :defer)
(use-package google-this :ensure t :defer)


;; Mail
;;
;; Configuration required by Gnus in the .emacs file.  These do not work when
;; set in .gnus as "Gnus" is mostly loaded from that file (see Info-goto-node
;; "gnus-parameters [gnus]").
(use-package gnus
  :commands gnus
  :init
  (setq gnus-treat-from-gravatar t
        mail-user-agent 'gnus-user-agent
        gnus-message-replysign t
        gnus-treat-x-pgp-sig t
        gnus-directory               "~/.emacs.d/gnus/"
        read-mail-command 'gnus-user-agent))

(use-package mm-decode
  :defer t
  :init
  (setq
   mm-coding-system-priorities '(utf-8 iso-latin-1 iso-latin-9 mule-utf-8)
   mm-verify-option 'always
   mm-decrypt-option 'always))

(use-package epa
  :defer t
  :init
  (setq epa-pinentry-mode 'loopback))

(use-package org

  :defer t
  :bind (([f6]   . org-capture)
         ;; inbox, anything scheduled can be seen w/ f8
         ([f7]   . org-todo-list)
         ([f8]   . org-agenda)
         ([C-f8] . org-agenda-kill-all-agenda-buffers)
         ("C-c C-/" . org-toggle-timestamp-type))


  :config
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  ;; executable from org
  ;; (setq org-babel-C-compiler
  (org-babel-do-load-languages
   'org-babel-load-languages
        '((emacs-lisp . t)
          (js . t)
          (haskell . t)
          (sqlite . t)
          (makefile . t)
          (scheme . t)
          (sql . t)
          (C . t)
          (sh . t)))

  :init
  (setq

   org-completion-use-ido t

   ;; org-archive-location "~/org/archive.org::*"
   ;;org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

   ;; after an item is scheduled, it will
   ;; show in the agenda on that day. this
   ;; is used to clear the inbox to categories
   org-default-notes-file "~/org/inbox.org"

   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :maxlevel . 3))

   org-hide-leading-stars nil

   org-src-fontify-natively t
   org-src-strip-leading-and-trailing-blank-lines t
   org-confirm-babel-evaluate nil
   ))

(use-package org-agenda
  :after (org)
  :hook
  (( org-capture-after-finalize . org-save-all-org-buffers ))
  (( org-capture-prepare-finalize . org-save-all-org-buffers ))

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


  :bind (:map org-agenda-mode-map
              ( "j"   . org-agenda-next-item )
              ( "k"   . org-agenda-previous-item ))

  :config
  (add-to-list 'org-agenda-custom-commands
        '("r" "inbox" tags "CATEGORY=\"inbox\"&LEVEL=2")))


(use-package bbdb
  :ensure t
  :defer
  :commands (bbdb)

  :bind (:map bbdb-mode-map
         ( "\t"  . bbdb-complete-mail ))

  :init
  (bbdb-mua-auto-update-init 'gnus 'message 'mail)
  (bbdb-initialize 'gnus 'message)
  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-message-all-addresses t)
  (setq bbdb-complete-mail-allow-cycling t)
  ;; 2000 is the default value which is added to a message's score if the
  ;; message is from a person in the BBDB database.
  (setq bbdb/gnus-score-default 2000)

  :config
  (evil-define-key 'motion bbdb-mode-map
    "\C-k"       'bbdb-delete-field-or-record
    "\C-x \C-s"   'bbdb-save)
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'message)) ;; use 'gnus for incoming messages too


(add-to-list 'default-frame-alist '(font . "monofur 12"))
(set-face-attribute 'default t :font '"monofur 12")

;; minimal modeline
;;
;; (set-face-attribute 'mode-line-emphasis :weight 1)
;; (set-face-attribute 'mode-line-highlight :background (x-get-resource "color2" ""))
;; (mode-line ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-silver))))
;; (mode-line-buffer-id ((t (:weight bold))))
;; (mode-line-inactive ((t (:background ,atom-one-dark-gray))))
(set-face-attribute 'mode-line nil :box '(:width 0.5))
(set-face-attribute 'mode-line-inactive nil :box nil)


;; extra emacs packages & utilities I use which aren't "core"
(use-package extra
  :if (file-exists-p "~/.emacs.d/lisp/extra.el")
  :load-path "lisp/")

(use-package extra-private
  :if (file-exists-p "~/.emacs.d/site-lisp/extra-private.el")
  :load-path "site-lisp/")
