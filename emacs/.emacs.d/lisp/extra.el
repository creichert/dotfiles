;;; extra.el --- -*- lexical-binding: nil; -*-

;;; Code:

(require 'use-package)


(use-package dockerfile-mode
  :defer
  :ensure t
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  (put 'dockerfile-build-args 'safe-local-variable #'sequencep)
  :mode ("\\.Dockerfile.\\'" . dockerfile-mode))


; set background color when rgb text is discovered
(use-package rainbow-mode
  :defer
  :ensure t)


(provide 'extra)

;;; extra.el ends here
