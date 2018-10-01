
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

;; minor modes
(global-font-lock-mode 1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(auto-compression-mode t)
(global-linum-mode -1)
(blink-cursor-mode 0)

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
(setq select-enable-clipboard t) ;; emacs 25.1


;; Use spaces instead of tabs, unless a mode/lang explicitly requires tabs.
;;
;; https://www.reddit.com/r/emacs/comments/19egbz/default_to_soft_tabs_enable_hard_tabs_only_for/
;;
(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq tab-always-indent t)
(setq fill-column 80)

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

;; (global-set-key (kbd "C-c S")       'first-error)


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
  :init
  (setq
   mail-user-agent  'gnus-user-agent
   read-mail-command 'gnus))


;; minibuffer history
;;
(use-package savehist
  :init
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))

  :config (savehist-mode 1))


(use-package ido
  :bind
  ("C-x f" . ido-find-file)
  :config
  (setq ido-ignore-files
        '("\\.rej$" "\\.dyn_hi$" "\\.dyn_o$" "\\.hi$" "\\.o$"
          "\\.tags$" "^\\.ghci$" "\\TAGS$" "\\#*#$")
        ido-max-directory-size 20000 ;2000000
        ido-enable-flex-matching t
        ido-max-prospects 5
        ido-create-new-buffer 'always
        ido-use-faces t
        ido-use-filename-at-point 'nil)
  (add-to-list 'ido-ignore-buffers "*Compile-Log*")
  (add-to-list 'ido-ignore-buffers "*Help*")
  (add-to-list 'ido-ignore-buffers "TAGS")
  (ido-mode 1)
  (ido-everywhere 1))


(use-package ido-vertical-mode
  :load-path "~/.emacs.d/ido-vertical-mode.el"
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))


(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode)
  :init
  (setq flx-ido-use-faces t)
  (setq flx-ido-threshold 1000))


(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode))


(use-package ido-at-point
  :ensure t
  :config
  (ido-at-point-mode))


(use-package ido-completing-read+
  :ensure t
  :after (ido)
  :init
  (setq completing-read-function 'ido-completing-read+
        ido-cr+-max-items nil)
  :config
  (ido-ubiquitous-mode t))


(use-package projectile
  :ensure t
  :bind
  ("C-x C-f" . projectile-find-file)
  ("C-x C-d" . projectile-switch-project)
  ([f5]      . projectile-compile-project)
  ("C-c g"   . projectile-grep)

  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
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
  :ensure t
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))


;; start in a state that immediately supports typing or direct emacs keybindings
(use-package etags
  :init
  (setq tags-revert-without-query 1)
  ;;(setq tags-case-fold-search nil) ;; case-insensitive
  (setq xref-etags-mode t)
  )

(use-package etags-select
  :load-path "~/.emacs.d/etags-select.el")


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


(use-package gif-screencast
  :ensure t
  :if window-system
  :config
  (print "gif-screencast loaded")
  (setq gif-screencast-screenshot-directory "~/downloads/screencasts/tmp")
  (setq gif-screencast-output-directory "~/downloads/screencasts")
  :bind
  (([f11] . gif-screencast)
   ([f12] . gif-screencast-stop)))


(use-package ansi-color
  :hook
  (( compilation-filter . (lambda ()
                            (let ((inhibit-read-only t))
                              (ansi-color-apply-on-region (point-min) (point-max)))) )))


(use-package compile
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
  :hook ((before-save
          . (lambda ()
              (when (not (memq major-mode (list
                                           "term-mode"
                                           "message-mode")))
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
  :hook ((with-editor-mode . evil-insert-state))
        ((with-presentation-mode . evil-insert-state))

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

  :config
  (define-key evil-motion-state-map "f" 'xref-find-definitions)
  (define-key evil-normal-state-map "s" 'pop-tag-mark)
  (define-key evil-normal-state-map ";" 'evil-ex)
  (define-key evil-normal-state-map (kbd "\\") 'smex)
  (define-key evil-insert-state-map (kbd "C-\\") 'smex)
  (define-key evil-insert-state-map "j" #'evil-maybe-exit)

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
  (define-key minibuffer-local-map            (kbd "C-w") 'evil-delete-backward-word)
  (define-key minibuffer-local-map            (kbd "C-p") 'evil-paste-after-from-0)
  (define-key minibuffer-local-map            (kbd "C-j") 'next-history-element)
  (define-key minibuffer-local-map            (kbd "C-k") 'previous-history-element)
  (define-key minibuffer-inactive-mode-map    (kbd "C-j") 'next-history-element)
  (define-key minibuffer-inactive-mode-map    (kbd "C-k") 'previous-history-element)
  (define-key minibuffer-local-ns-map         (kbd "C-j") 'next-history-element)
  (define-key minibuffer-local-ns-map         (kbd "C-k") 'previous-history-element)
  (define-key minibuffer-local-isearch-map    (kbd "C-j") 'next-history-element)
  (define-key minibuffer-local-isearch-map    (kbd "C-k") 'previous-history-element)
  (define-key minibuffer-local-completion-map (kbd "C-j") 'next-history-element)
  (define-key minibuffer-local-completion-map (kbd "C-k") 'previous-history-element)
  (define-key minibuffer-local-must-match-map (kbd "C-j") 'next-history-element)
  (define-key minibuffer-local-must-match-map (kbd "C-k") 'previous-history-element)
  (define-key minibuffer-local-must-match-map (kbd "C-k") 'previous-history-element)

  (defun evil-paste-after-from-0 ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))

  (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

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
    "e"       'flycheck-next-error
    "w"       'flycheck-previous-error
    ")"       'evil-next-close-paren
    "o"       'other-window
    "xc"      'save-buffers-kill-terminal
    "f"       'projectile-find-file
    "prt"     'projectile-regenerate-tags
    "c"       'projectile-compile-project
    "v"       'recompile-quietly ;; still under projectile context
    "k"       'recompile-quietly ;; still under projectile context
    "ptt"     'projectile-toggle-between-implementation-and-test
    "tc"      'toggle-compilation-visible
    "b"       'projectile-switch-to-buffer
    ;; currently overlapping to see which i prefer
    "("       'insert-parentheses
    "9"       'insert-parentheses
    "U"       'browse-url-chromium
    "gpgr"    'epa-sign-region
    "gpgf"    'epa-sign-file
    "gpgvr"   'epa-verify-region
    "gpgvf"   'epa-verify-file
    "G"       'google-this
    "isw"     'ispell-word
    ))


(use-package w3m
 :ensure t
 :init (setq
        browse-url-generic 'browse-url-browser-function
        browse-url-default-browser 'w3m-browse-url ;; browse-url-emacs
        browse-url-browser-function
        '(("github" . browse-url-chromium)
          ("." . browse-url-default-browser))))


(use-package magit
  :ensure t
  :bind (([f9]   . magit-status)
         ([C-f9] . magit-log))
  :init (setq magit-completing-read-function 'magit-ido-completing-read)
  :after (evil evil-leader))


(use-package evil-magit
  :ensure t
  :after (magit evil)
  :config
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
    "z" 'magit-key-mode-popup-stashing)
  (evil-define-key evil-magit-state magit-mode-map
    "p" 'magit-section-backward
    "n" 'magit-section-forward)
  (evil-leader/set-key-for-mode 'magit-status-mode
    "SPC" 'magit-stash-show)
  )


(use-package flyspell
 :hook ((prog-mode . flyspell-prog-mode)))

(use-package flycheck
  :ensure t
  :hook ((after-init . global-flycheck-mode))
  :init (setq flycheck-standard-error-navigation nil)
  )


(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :interpreter
  ("stack"      . haskell-mode)
  ("runhaskell" . haskell-mode)
  :init
  (setq haskell-process-args-stack-ghci
        '("--no-load"
          "--ghci-options=-O0"
          "--ghci-options=-ferror-spans"
          "--ghci-options=-fshow-loaded-modules"
          ))
  (setq haskell-stylish-on-save t
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
        haskell-indentation-starter-offset 4)

  :hook ((haskell-mode . haskell-doc-mode))
        ((haskell-mode . haskell-decl-scan-mode))
        ((haskell-mode . haskell-indentation-mode))
  :config
  ;; https://gist.github.com/989ad8be92f68682abff
  (defun haskell-run-function-under-cursor ()
    "Send the word-at-point as a function to GHCi process."
    (interactive)
    ;; (haskell-process-set-sent-stdin 't)
    (haskell-process-send-string
     (haskell-session-process (haskell-session-maybe))
     (format "%s" (word-at-point))))
  ;;(require 'evil-leader)
  (evil-leader/set-key-for-mode 'haskell-mode
    "hir" 'hindent-reformat-region
    "hid" 'hindent-reformat-decl-or-fill
    "f" 'haskell-mode-jump-to-def-or-tag
    "l" 'haskell-process-load-or-reload)

  :bind (:map haskell-mode-map
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-;" . haskell-process-load-file)
              ("C-c C-l" . haskell-process-reload)
              ("C-c i"   . haskell-navigate-imports-go)
              ("C-c I"   . haskell-navigate-imports-return)
              ("C-c C-j" . haskell-run-function-under-cursor))

  )


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
 :init
  (setq sql-postgres-options '("--no-psqlrc"))
  (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
  (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] "))


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


(use-package yaml-mode :ensure t)


(use-package google-this :ensure t)


;; Mail
;;
;; Configuration required by Gnus in the .emacs file.  These do not work when
;; set in .gnus as "Gnus" is mostly loaded from that file (see Info-goto-node
;; "gnus-parameters [gnus]").
(use-package gnus
  :commands gnus
  ;; :defer 5
  :init
  (setq gnus-treat-from-gravatar t
        mail-user-agent 'gnus-user-agent
        gnus-message-replysign t
        gnus-treat-x-pgp-sig t
        gnus-directory               "~/.emacs.d/gnus/"
        read-mail-command 'gnus-user-agent))

(use-package mm-decode
  :init
  (setq
   mm-coding-system-priorities '(utf-8 iso-latin-1 iso-latin-9 mule-utf-8)
   mm-verify-option 'always
   mm-decrypt-option 'always))

(use-package epa
  :init
  (setq epa-pinentry-mode 'loopback))

(use-package term
  :init

(defun remote-term (new-buffer-name cmd &rest switches)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))

(defun ssh-shell (host)
  (interactive "sHost: \n")
  (remote-term (format "ssh-%s" host) "ssh" (format "%s" host))))


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
