
;;; creichert emacs configuration

;;; Code:

;; bootstrap use-package

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package bind-key :ensure t :demand)
;; (setq use-package-expand-minimally t)
(setq use-package-compute-statistics t)

;; configure emacs
(set-frame-font "Hack Nerd Font Mono" nil t)


;; Ensure system executables are installed for certain packages.
(setq custom-file "~/.emacs.d/custom.el")
(setq frame-title-format "emacs - %b")
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
(setq confirm-kill-emacs 'y-or-n-p)

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
  :defer
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
  (size-indication-mode)
  (column-number-mode)
  :init
  (setq
   mail-user-agent  'gnus-user-agent
   read-mail-command 'gnus))


(use-package savehist
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 30)
  :init
  (setq savehist-additional-variables
        '( ;;kill-ring  adds photos & other very large files
          compile-command
          projectile-project-command-history
          search-ring
          regexp-search-ring))
  (setq savehist-file "~/.emacs.d/history")
  :config
  (savehist-mode 1))


(use-package vc
  :defer
  :custom
  (vc-follow-symlinks t))


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;;(load-theme 'doom-city-lights t)
  ;;(load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package ido
  :demand
  :ensure t
  :bind
  (("C-x f" . ido-find-file)
   ("C-c C-x C-o" . ido-switch-buffer-other-window))
  :config
  (add-to-list 'ido-ignore-files "\\.tags$")
  (ido-mode 1)
  (ido-everywhere 1)
  :init
  (setq ido-max-directory-size 100000
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-faces t
        ido-use-filename-at-point 'nil))


(use-package ido-vertical-mode
  :load-path "~/.emacs.d/site-lisp/ido-vertical-mode.el/"
  :requires (ido)
  :config (ido-vertical-mode)
  :custom
  (ido-vertical-define-keys 'C-n-and-C-p-only))


(use-package flx-ido
  :ensure t
  :requires (ido)
  :config (flx-ido-mode)
  :custom
  (flx-ido-use-faces t)
  (flx-ido-threshold 1000))


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
  (setq completing-read-function 'ido-completing-read+))


(use-package xref
  ;:defer
  :requires (ido)
  :preface
  (defun bury-xref-buffer ()
    (delete-windows-on "*xref*"))
  :config
  (defun xref-show-definitions-ido (fetcher alist)
    "Use ido for xref, skip prompt for single match, replace current buffer."
    (let* ((xrefs (funcall fetcher))
           (summaries (mapcar (lambda (x) (slot-value x 'summary)) xrefs)))
      (if (= (length xrefs) 1)
          (xref--show-def-in-current-window (car xrefs))
        (let ((selected (ido-completing-read "Definitions: " summaries nil t)))
          (when selected
            (let ((xref (nth (cl-position selected summaries :test #'equal) xrefs)))
              (xref--show-def-in-current-window xref)))))))

  (defun xref--show-def-in-current-window (xref)
    "Show XREF in current window, center point."
    (let* ((location (slot-value xref 'location))
           (buffer (or (and (fboundp 'xref-location-marker)
                            (marker-buffer (xref-location-marker location)))
                       (find-file-noselect (xref-location-file location)))))
      (pop-to-buffer buffer '((display-buffer-same-window)))
      (xref--goto-location location)
      (recenter nil t)))

  (setq xref-show-definitions-function #'xref-show-definitions-ido)
  (add-to-list 'xref-after-return-hook 'bury-xref-buffer))


(use-package projectile
  :ensure t
  :requires (ido)
  ;:hook (( projectile-after-switch-project . magit-status ))
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
        projectile-tags-command "make tags"
        projectile-use-git-grep 't
        projectile-globally-ignored-directories '("~/.stack/snapshots")
        ;;projectile-project-search-path '("~/dev")
        ; ignore projects added from jumping to tags
        ;projectile-ignored-projects '("~/.stack/snapshots/*/*/*/*/*")
        projectile-ignored-project-function
        (lambda (path)
          ;(string-match "\\(:?\\`/\\(:?nix\\|tmp\\)\\|/\\.nix-profile\\)" path))
          ;(string-match ".emacs.d/elpa/*" path)
          (string-match ".stack/snapshots" path))
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
  (("M-x"  . smex)
   ("C-\\" . smex)
   ("M-X"  . smex-major-mode-commands)))


;; start in a state that immediately supports typing or direct emacs keybindings
(use-package etags
  :defer
  :custom
  (tags-case-fold-search nil)
  (tags-revert-without-query t)
  (tags-add-tables t))


(use-package compile
  :defer
  :ensure t
  ;;:hook ((compilation-mode . (lambda () (setq scroll-margin 0))
  :init

  (use-package ansi-color
    :defer
    :hook
    (( compilation-filter . (lambda ()
                              (let ((inhibit-read-only t))
                                (ansi-color-apply-on-region (point-min) (point-max)))) )))

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

  ;; TODO how to make this less complicated?
  ;; NOTE currently required, do not touch without a clean git history
  (eval-after-load 'compile
    (lambda ()
      ;; This lets next-error find errors generated under stack's
      ;; --interleaved-output. The error from ghc is prefixed with
      ;; "package-name> ". After that, the rest of the pattern comes from the
      ;; "gnu" item in `compilation-error-regexp-alist-alist'. I stripped off
      ;; the start of that pattern, which I think was matching program names, e.g.
      ;;     gcc: filename:101:3
      ;; It's still more complicated than necessary, but I didn't want to mess
      ;; with it too much.
      (let ((pat "\\(?:[[:alnum:]-] *> \\)\\(?1:\\(?:[0-9]*[^0-9\n]\\)\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\)\\(?:: ?\\)\\(?2:[0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\(?:\\.\\(?5:[0-9]+\\)\\)?\\|[.:]\\(?3:[0-9]+\\)\\(?:-\\(?:\\(?4:[0-9]+\\)\\.\\)?\\(?5:[0-9]+\\)\\)?\\)?:\\(?: *\\(?6:\\(?:FutureWarning\\|RuntimeWarning\\|W\\(?::\\|arning\\)\\|warning\\)\\)\\| *\\(?7:[Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|\\[ skipping \\.+ ]\\|instantiated from\\|required from\\|[Nn]ote\\)\\| *\\(?:[Ee]rror\\)\\|[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"))
        (add-to-list 'compilation-error-regexp-alist-alist
                     `(stack-interleaved ,pat 1 (2 . 4) (3 . 5) (6 . 7)))
        (add-to-list 'compilation-error-regexp-alist 'stack-interleaved))))

  :config

  (setq scroll-margin 0
        scroll-step 1
        scroll-conservatively 10000)

  (defvar compilation-buffer-visible t)

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
  :hook ((prog-mode . whitespace-local-mode)
         (conf-mode . whitespace-local-mode)
         ))


(use-package register
  :preface
  (defun creichert/load-window-disposition (&optional regnum)
    (interactive "PRegister number (default is 9): ")
    (jump-to-register (or regnum 9)) ;; TODO prefix the register
    (message "Windows disposition loaded"))
  (defun creichert/save-window-disposition (&optional regnum)
    (interactive "PRegister number (default is 9): ")
    (window-configuration-to-register (or regnum 9))
    (message "Windows disposition saved"))
  :commands (creichert/load-window-disposition creichert/save-window-disposition)
  :bind (([C-f10] . creichert/save-window-disposition)
         ([f10]   . creichert/load-window-disposition)))


(use-package evil
  :ensure t
  :hook
  ((with-editor-mode . evil-insert-state))
  ((with-presentation-mode . evil-motion-state))
  ;;((special-mode . evil-emacs-state))
  ((archive-mode . evil-motion-state))
  ((sql-interactive-mode . evil-motion-state))
  ((prog-mode . (lambda ()
                  (progn
                    (defalias #'forward-evil-word #'forward-evil-symbol)))))
  ;;:config
  ;;((xref--show-xref-buffer-mode . evil-emacs-state))
  ;;((prog-mode . (lambda ()
  ;;                (defalias #'forward-evil-word #'forward-evil-symbol))

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
              ("s" . xref-go-back)
              :map evil-normal-state-map
              ("f" . xref-find-definitions)
              ("s" . xref-go-back)
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
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-default-state 'normal)
  ;;(setq-default evil-kill-on-visual-paste nil)
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
    "O"       'previous-window-any-frame

    "a"       'fill-paragraph
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
    "gpgsr"   'epa-sign-region
    "gpgsf"   'epa-sign-file
    "gpger"   'epa-encrypt-region
    "gpgef"   'epa-encrypt-file
    "gpgvr"   'epa-verify-region
    "gpgvf"   'epa-verify-file

    "isw"     'ispell-word

    "u"       'browse-url
    "U"       'browse-url-chromium
    ;;"G"       'google-this ;; TODO Fix

    "x"       'gptel-menu
    ))


;; (use-package w3m
;;   :ensure t
;;   :commands (w3m-browse-url w3m-find-file)
;;   ;;:ensure-system-package ("w3m")
;;   :preface
;;   ;; (defun browse-url-chromium (url &optional _new-window)
;;   (defun browse-url-prompt (browser-name)
;;     (interactive (list (completing-read "Select browser: " '("chromium" "w3m" "firefox" "chrome"))))
;;     (message (format "browser: %s" browser-name))
;;     (pcase browser-name
;;      ("chromium" 'browse-url-chromium)
;;      ("w3m" 'w3m-browse-url)
;;      ("chrome" 'browse-url-chrome)
;;      ("firefox" 'browse-url-firefox)
;;      (_ 'browse-url-chromium)))
;;   :init (setq
;;          browse-url-handlers
;;          '(("github.com" . browse-url-chromium)
;;            ("trello.com" . browse-url-chromium)
;;            ("circleci.com" . browse-url-chromium)
;;            ("pagerduty.com" . browse-url-chromium)
;;            ("accounts.google.com" . browse-url-chromium)
;;            ("accounts.spotify.com" . browse-url-chromium)
;;            ("assertible.com" . browse-url-chromium)
;;            ("simplyrets.com/admin" . browse-url-chromium)
;;            ("slack.com" . browse-url-chromium)
;;            ("rollbar.com" . browse-url-chromium)
;;            ("app.drift.com" . browse-url-chromium)
;;            ("gmail.com" . browse-url-chromium)
;;            ("aws.amazon.com" . browse-url-chromium)
;;            ("youtube.com" . browse-url-chromium)
;;            ("facebook.com" . browse-url-chromium)
;;            ("upwork.com" . browse-url-chromium)
;;            (".*\\.gov" . browse-url-chromium)
;;            ("docusign.com\\|docusign.net" . browse-url-chromium)
;;            ("." . (lambda (url &optional args)
;;                     (lexical-let ((browserf (call-interactively #'browse-url-prompt)))
;;                       (funcall browserf url args)))))))



(use-package flycheck
  :ensure t
  :hook ((after-init . global-flycheck-mode))
  ;;:ensure-system-package ((proselint . "pip install proselint"))
  :config
  ;(setenv "USEIDE" "true")
  (setq flycheck-standard-error-navigation nil)
  (setq flycheck-checker-error-threshold 10000)
  ;;(flycheck-define-checker proselint
  ;;  "A linter for prose."
  ;;  :command ("proselint" source-inplace)
  ;;  :error-patterns
  ;;  ((warning line-start (file-name) ":" line ":" column ": "
  ;;            (id (one-or-more (not (any " "))))
  ;;            (message) line-end))
  ;;  ;; doesn't work well with org-mode
  ;;  :modes (message-mode text-mode markdown-mode gfm-mode)
  ;;  :custom
  ;;  (flycheck-emacs-lisp-load-path 'inherit)
  ;;  )
  )


; (use-package flyspell
;   :requires (flycheck)
;   :hook ((markdown-mode . turn-on-flyspell))
;         ((prog-mode     . flyspell-prog-mode)))


(use-package sql
  :defer
  :preface
  (defun c/setup-pgsql-buffer ()
    ;; (bug) make comint use entire buffer
    (setq truncate-lines t))

  :config
  (add-to-list 'sql-postgres-options "--no-psqlrc")
  (add-to-list 'sql-postgres-options "--expanded")
  (add-to-list 'sql-postgres-options "--echo-queries")
  (add-to-list 'sql-postgres-options "--pset=null=[NULL]")
  ; can't get working w/ dbname
  ;(add-to-list 'sql-postgres-options "--variable=HISTFILE=~/.cache/psql_history-:DBNAME")
  (add-to-list 'sql-postgres-options "--variable=HISTFILE=/dev/null")
  :hook
  ((sql-interactive-mode-hook . c/setup-pgsql-buffer))
  )


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
  :mode ("\\.*rc\\'" "\\.*conf\\'"))


(use-package markdown-mode
  :ensure t
  ;;:ensure-system-package ("markdown")
  :mode ("\\.markdown\\'" . markdown-mode))


(use-package dotenv-mode
  :ensure t
  :mode ("\\.env.sample" . dotenv-mode))


(use-package yaml-mode
  :ensure t :defer)


;(use-package flycheck-yamllint
;  :ensure-system-package (yamllint)
;  :ensure t :defer)


(use-package auth-source-pass
  :ensure t
  :defer
  :config
  (auth-source-pass-enable))


;(use-package pinentry)
;(require 'pinentry)
;(pinentry-start)
;(setenv "INSIDE_EMACS" "YES")
(use-package epg
  :defer
  :ensure-system-package (gpg2 . gnupg2)
  :custom
  (epg-debug t)
  :config
  (setq epg-pinentry-mode 'loopback))


(use-package mood-line
  :ensure t
  :init
  (mood-line-mode))


;; (use-package org-settings
;;   :load-path "lisp/")


;; (use-package bbdb-settings
;;   :load-path "lisp/")


(use-package haskell-settings
  :load-path "lisp/")


;; (use-package web-settings
;;   :load-path "lisp/")

(use-package git-settings
  :load-path "lisp/")


;; extra emacs packages & utilities I use which aren't "core"
(use-package extra
  :load-path "lisp"
  :if (file-exists-p "~/.emacs.d/lisp/extra.el"))
