
(require 'use-package)


(use-package magit
  :defer
  :ensure t
  :bind (([f9]   . magit-status)
         ([C-f9] . magit-log))
  :requires (evil)
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-section-initial-visibility-alist
        '((stashes   . hide)
          (untracked . hide)
          (pulls . show)
          (unpushed . show)
          (commit . show)
          (status . show)
          ))

  (use-package magit-ido :ensure t
    :init
    (setq magit-completing-read-function 'magit-ido-completing-read))

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
    "l" 'magit-log
    "h" 'magit-diff-toggle-refine-hunk
    )

  (evil-leader/set-key-for-mode 'magit-status-mode
    ;"SPC" 'magit-visit-ref
    "H" 'magit-go-backward)
  (evil-leader/set-key-for-mode 'magit-diff-mode
    ;"SPC" 'magit-visit-ref
    "H" 'magit-go-backward)
  (evil-leader/set-key-for-mode 'magit-revision-mode
    ;"SPC" 'magit-visit-ref
    "H" 'magit-go-backward)
  (evil-leader/set-key-for-mode 'magit-log-mode
    ;"SPC" 'magit-visit-ref
    "H" 'magit-go-backward)
  )


(use-package evil-collection
  :ensure t
  :after (magit evil)
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


;; (use-package magithub
;;   :disabled
;;   :after magit
;;   :init
;;   (use-package auth-source-pass
;;     :ensure t :demand
;;     :init (auth-source-pass-enable))
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/code"))


;; (use-package magit-gh-pulls
;;   :disabled
;;   :ensure t
;;   :hook (magit-mode . magit-gh-pulls-mode)
;;   :init
;;   (use-package magit-popup
;;     :ensure t)
;;   (setq magit-gh-pulls-pull-detail-limit 30)
;;   (setq magit-gh-pulls-status-documentation t))


;; (use-package github-clone
;;   :defer
;;   :ensure t)
;;
;;
;; (use-package gist
;;   :defer
;;   :ensure-system-package (git)
;;   :ensure t)



(provide 'git-settings)

;;; bbdb-settings.el ends here
