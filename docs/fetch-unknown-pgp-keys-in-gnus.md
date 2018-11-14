---
title: Fetch unknown PGP keys in Gnus
author: creichert
date: 2018-10-20
layout: post
---

If you use Gnus for Emacs to manage your mail, and have enabled "buttonized" PGP
parts, you may eventually come across a signature that you don't have the public
key for:

    "[[PGP Signed Part:No public key for

While fetching the key from the command-line or your PGP client is not
difficult, it's much easier to have a simple keybinding that fetches the public
key directly from Gnus.

I've written the following elisp function to solve this problem, originally
inspired by a discussion in the Gnus mailing list[^0]:

```elisp
(use-package gnus
  :preface
  (defun gnus-article-receive-epg-keys ()
    "Fetch unknown PGP public keys for a signed a signed message. [[PGP Signed Part: ... ]]"
    (interactive)
    (with-current-buffer gnus-article-buffer
      (save-excursion
        (goto-char (point-min))
        (if
            (re-search-forward "\\[\\[PGP Signed Part:No public key for \\([A-F0-9]\\{16,16\\}\\) created at "
                               nil 'noerror)
            (shell-command (format "gpg --keyserver %s --recv-keys %s"
                                   "keyserver.ubuntu.com"
                                   (match-string 1)))
          (message "No unknown signed parts found."))
        (gnus-summary-show-article))))

  :bind (:map gnus-summary-mode-map
         ("C-c k" . gnus-article-receive-epg-keys)
         :map gnus-article-mode-map
         ("C-c k" . gnus-article-receive-epg-keys))

  :init
  (setq gnus-treat-x-pgp-sig t))
```

Now, in the Gnus summary or article buffer, simply hit `C-c k` when you want to
fetch and validate the signature for a mail when it's missing.

- C

[^0]: https://lists.gnu.org/archive/html/info-gnus-english/2012-10/msg00034.html
