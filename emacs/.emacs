
;; creichert emacs configuration

(require 'cl)
(require 'package)

(defvar required-packages
  '(
    ;; evil
    evil
    evil-leader

    ;; menus & commands
    smex
    ido-at-point
    ido-completing-read+
    flx-ido

    ;; project management / ide
    projectile
    flycheck

    ;; git
    magit
    evil-magit
    gist

    ;; haskell
    haskell-mode
    flycheck-haskell
    hindent

    ;; misc modes
    web-mode
    markdown-mode
    dockerfile-mode
    yaml-mode
    ansi-color
    dotenv-mode

    xresources-theme
    gif-screencast
    )
  "installed on startup"
)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ))

;; Needed to use melpa packages in this module
(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun load-local-package (dir)
  "Add DIR git repo to 'load-path'."
  (add-to-list 'load-path
               (concat user-emacs-directory
                     ;; Guarantee a valid filename across all OSs.
                     (convert-standard-filename dir))))

;;; core
;;
;;
(load-local-package "ido-vertical-mode.el")
(load-local-package "etags-select.el/")

;; display
;;
;; minimal default display
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


(setq-default fill-column 80)

;; Write backup and auto-save files to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; minibuffer history
;;
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun reload-dotemacs ()
  "Reload init file without restarting Emacs."
  (interactive)
  (load-file user-init-file))

(global-unset-key (kbd "C-z"))  ; don't allow freeze
(global-unset-key (kbd "C-x C-z"))  ; don't allow freeze

(global-set-key (kbd "C-c :")       'reload-dotemacs)
(global-set-key (kbd "C-x f")       'ido-find-file)
;; (global-set-key (kbd "C-c E")       'first-error)
;; (global-set-key (kbd "C-c S")       'first-error)
(global-set-key (kbd "C-c e")       'next-error)
(global-set-key (kbd "C-c C-e p ")  'previous-error)

(global-set-key (kbd "C-x /")       'align-regexp)
(global-set-key (kbd "C-x C-/")     'replace-regexp)
(global-set-key (kbd "C-c :")       'reload-dotemacs)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c b")       'execute-extended-command)

(global-set-key (kbd "C-x C-f")     'projectile-find-file)
(global-set-key (kbd "C-x C-d")     'projectile-switch-project)
(global-set-key [f5]                'projectile-compile-project)
(global-set-key (kbd "C-c g")       'projectile-grep)

;; vim-like bindings in the minibuffer
(define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
(define-key minibuffer-local-map (kbd "C-p") 'evil-paste-after-from-0)
(define-key minibuffer-inactive-mode-map (kbd "C-j") 'next-history-element)
(define-key minibuffer-inactive-mode-map (kbd "C-k") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-j") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-k") 'previous-history-element)
(define-key minibuffer-local-completion-map (kbd "C-j") 'next-history-element)
(define-key minibuffer-local-completion-map (kbd "C-k") 'previous-history-element)
(define-key minibuffer-local-must-match-map (kbd "C-j") 'next-history-element)
(define-key minibuffer-local-must-match-map (kbd "C-k") 'previous-history-element)
(define-key minibuffer-local-must-match-map (kbd "C-k") 'previous-history-element)

(add-to-list 'auto-mode-alist '("\\.bash_functions.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_functions\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'"   . sh-mode))
(add-to-list 'auto-mode-alist '("\\.json.template\\'"  . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.inputrc\\'"        . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'"            . conf-mode))

(require 'flx-ido)
(require 'ido-vertical-mode)
(require 'ido-at-point)

(ido-mode 1)
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode t)
(ido-at-point-mode)

(eval-after-load 'ido-ubiquitous
  '(append '(enable prefix "xref-") ido-ubiquitous-command-overrides))

(flx-ido-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'projectile)
(projectile-mode)

(setq projectile-enable-caching t
      projectile-indexing-method 'alien
      projectile-use-git-grep 't
      projectile-project-search-path '("~/dev")
      projectile-globally-ignored-directories '("~/.stack/snapshots")
      projectile-tags-command "make tags")


(add-to-list 'ido-ignore-buffers "*Compile-Log*")
(add-to-list 'ido-ignore-buffers "*Help*")
(add-to-list 'ido-ignore-buffers "TAGS")

(setq ido-ignore-files '(
                        "\\.rej$"
                        "\\.dyn_hi$"
                        "\\.dyn_o$"
                        "\\.hi$"
                        "\\.o$"
                        "\\.tags$"
                        "^\\.ghci$"
                        "\\TAGS$"
                        "\\#*#$"
                        ))

(setq dired-omit-files
      (concat
       "|\\TAGS\\" "|\\#$\\" "|\\.*~$\\"
       "|\\.dyn_hi$\\" "|\\.dyn_o$\\"
       "|\\.hi$\\" "|\\.o$"
       ))

(setq ido-max-directory-size 20000 ;2000000
      ido-enable-flex-matching t
      ido-max-prospects 5
      ido-create-new-buffer 'always
      ido-use-faces t
      ido-use-filename-at-point 'nil
      flx-ido-use-faces t
      flx-ido-ereshold 1000
      )

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


(setq scroll-conservatively 1000)
(setq scroll-margin 7)

(add-hook 'compilation-mode-hook
          (lambda ()
            (setq scroll-margin 0)))

(require 'dotenv-mode)
(add-to-list 'auto-mode-alist '("\\.env.\\'" . dotenv-mode))
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("\\.Dockerfile.\\'" . dockerfile-mode))



;; Xresource based styles
;;
;; Uses colors supplied through xresources (xrdb -query) to make emacs
;; consistent with other desktop applications. In theory, all apps should use
;; this but "larger" apps like Chrome & Gnome apps will often just ignore it.
;; This is only needed once, near the top of the file
(condition-case nil
               (progn
                 (require 'xresources-theme)
                 (load-theme 'xresources t))
               (error nil))

(add-to-list 'default-frame-alist '(font . "monofur 12"))
(set-face-attribute 'default t :font '"monofur 12")


;; minimal modelin
;;
;; (set-face-attribute 'mode-line-emphasis :weight 1)
;; (set-face-attribute 'mode-line-highlight :background (x-get-resource "color2" ""))
;; (mode-line ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-silver))))
;; (mode-line-buffer-id ((t (:weight bold))))
;; (mode-line-inactive ((t (:background ,atom-one-dark-gray))))
(set-face-attribute 'mode-line nil :box '(:width 0.5))
(set-face-attribute 'mode-line-inactive nil :box nil)

(condition-case nil
               (progn
                 (require 'gif-screencast)
		 (print "gif-screencast loaded")
                 (setq gif-screencast-screenshot-directory "~/downloads/screencasts/tmp")
                 (setq gif-screencast-output-directory "~/downloads/screencasts")
		 (global-set-key [f11]   'gif-screencast)
		 (global-set-key [f12]   'gif-screencast-stop)
		 )
               (error (print "error loading gif-screencast")))


(require 'ansi-color)

(setq compilation-read-command nil
      compilation-scroll-output t)

(evil-leader/set-key
  "v" 'recompile-quietly
  "k" 'recompile-quietly
  )

;; Fix some escape sequences in compilation buffer for complex output
;; like webpack
(add-hook 'compilation-mode-hook
	  (lambda ()
	    (read-only-mode)
	    (ansi-color-apply-on-region (point-min) (point-max))
	    ;;(ansi-color-apply-on-region compilation-filter-start (point))
	    (toggle-read-only)
	    ))

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

(defun notify-compilation-result(buffer msg)
  (if (and (string-match "^finished" msg) (string= "*compilation*" buffer))
      (progn
	;; frame-visible-p before... to determine whehter to add it
	(delete-windows-on buffer)
	(message "compilation successful" buffer))
    (message "compilation failed: " (buffer-substring (- (point-max) 500) (point-max))))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (raise-frame current-frame))

(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (projectile-compile-project nil)))

;; make compilation-mode a lot faster but excluding cpu intensive regexp's which
;; clog up the buffer on long lines.
(setq compilation-error-regexp-alist
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


(require 'whitespace)

(add-hook 'prog-mode-hook
           (lambda ()
             (add-hook 'before-save-hook 'delete-trailing-whitespace)
	     ))

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

(require 'evil)
(require 'evil-leader)
(require 'evil-commentary)

;; global-evil-leader-mode is enabled before evil-mode so initial buffers
;; (*scratch*, *Messages*, …)  are configured correctly.
(global-evil-leader-mode 1)
(evil-mode 1)

(setq evil-leader/leader "SPC"
      evil-leader/in-all-states t
      evil-leader/non-normal-prefix "C-"
      evil-leader/no-prefix-mode-rx
      '("magit-.*-mode"
	"*Messages*"
	"gnus-.*-mode"))


;;(evil-set-toggle-key "C-a")
(setq evil-default-state 'normal)
(setq undo-tree-enable-undo-in-region 'nil)

;; start in a state that immediately supports typing or direct emacs keybindings
(add-hook 'with-editor-mode-hook 'evil-insert-state)
(add-hook 'with-presentation-mode-hook 'evil-insert-state)

(setq browse-url-browser-function 'browse-url-chromium)

(evil-leader/set-key
  "e"		'flycheck-next-error
  "w"		'flycheck-previous-error
  ")"		'evil-next-close-paren
  "o"		'other-window
  "xc"		'save-buffers-kill-terminal

  "f"		'projectile-find-file
  "prt"		'projectile-regenerate-tags
  "c"		'projectile-compile-project
  "b"		'projectile-switch-to-buffer

  ;; currently overlapping to see which i prefer
  "("		'insert-parentheses
  "9"		'insert-parentheses

  "U"		'browse-url-chromium

  "gpgr"	'epa-sign-region
  "gpgf"	'epa-sign-file
  "gpgvr"	'epa-verify-region
  "gpgvf"	'epa-verify-file
  )


(require 'etags-select)

(setq tags-revert-without-query 1)
(setq tags-case-fold-search nil) ;; case-insensitive
(setq tags-add-tables t)


(define-key evil-motion-state-map "f" 'find-tag)
;;(define-key evil-motion-state-map "f" 'haskell-mode-jump-to-def-or-tag)
;;(define-key evil-motion-state-map "f" 'etags-select-find-tag)
;;(define-key evil-motion-state-map "f" 'my-ido-find-tag)

(define-key evil-normal-state-map "s" 'pop-tag-mark)
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map (kbd "\\") 'smex)
(define-key evil-insert-state-map (kbd "C-\\") 'smex)
(define-key evil-insert-state-map "j" #'evil-maybe-exit)

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

;; magit

(require 'magit)
(require 'evil-magit)

(global-set-key [f9]   'magit-status)
(global-set-key [C-f9] 'magit-log)
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq git-commit-summary-max-length 73)

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


;; flycheck

(require 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-standard-error-navigation nil)


;; haskell

(require 'haskell-mode)

(setq haskell-process-reload-with-fbytecode nil
      haskell-process-use-presentation-mode t
      haskell-process-type 'stack-ghci
      haskell-stylish-on-save t
      ;; haskell-process-log t
      ;; haskell-mode-stylish-haskell-path "brittany"
      haskell-indent-spaces 4)

;; https://gist.github.com/989ad8be92f68682abff
(defun haskell-run-function-under-cursor ()
  "Send the word-at-point as a function to GHCi process."
  (interactive)
  ;; (haskell-process-set-sent-stdin 't)
  (haskell-process-send-string
            (haskell-session-process (haskell-session-maybe))
	    (format "%s" (word-at-point))))

(add-to-list 'interpreter-mode-alist '("stack"      . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(require 'flycheck-haskell)
(setq flycheck-ghc-args '("-Wall"))
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(add-hook 'haskell-mode-hook
	  (lambda ()

            (haskell-doc-mode)
            (haskell-decl-scan-mode)

            (setq haskell-process-args-stack-ghci
                     '("--no-load"
                       "--ghci-options=-O0"
                       "--ghci-options=-ferror-spans"
                       "--ghci-options=-fshow-loaded-modules"
	    	       ))

	    ;;(define-key evil-motion-state-map "f" 'haskell-mode-tag-fine)
            ;;(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
            (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
            (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
            (define-key haskell-mode-map (kbd "C-c C-;") 'haskell-process-load-file)
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-reload)
            (define-key haskell-mode-map (kbd "C-c i")   'haskell-navigate-imports-go)
            (define-key haskell-mode-map (kbd "C-c I")   'haskell-navigate-imports-return)
            (define-key haskell-mode-map (kbd "C-c C-j") 'haskell-run-function-under-cursor)

	    (require 'hindent)
            (turn-on-haskell-indentation)
	    (hindent-mode)

	    (evil-leader/set-key-for-mode 'haskell-mode
	      "hir" 'hindent-reformat-region
	      "hid" 'hindent-reformat-decl-or-fill
	      "f" 'haskell-mode-jump-to-def-or-tag
	      "l" 'haskell-process-load-or-reload
	      )
            ))

;;; Web-mode setup

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-indent-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-indent-hook)
(add-hook 'web-mode-hook  'emmet-mode)

;;; Flycheck setup

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

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

(defun web-mode-init-hook ()
  ;;(prettier-js-mode)
  ;; (setq prettier-js-args '("--tab-width" "4"
  ;;                          "--trailing-comma" "es5"
  ;;                          "--print-width" "90"
  ;;                          "--jsx-bracket-same-line" "true"
  ;;                          "--no-semi"
  ;;                          ))
  )

(add-hook 'web-mode-hook  'web-mode-init-hook)
(add-hook 'flycheck-mode-hook 'use-eslint-from-node-modules)

;; sql

(require 'sql)
;; (setq sql-postgres-options '("--prompt=psql> "))
;; (sql-set-product-feature 'postgres :prompt-regexp "^\\(Postgrj\\) \\[[_a-zA-Z]*\\]> ")
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-postgres-options '("--no-psqlrc"))
            (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
            (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

(setq custom-file "~/.emacs.d/custom.el")
