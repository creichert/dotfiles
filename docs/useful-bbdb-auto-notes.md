---
title: Useful BBDB v3 auto notes rules
author: creichert
---

BBDB is a plain-text database format for storing contact information that is
prominent in the Emacs community. I use BBDB to store and access contact
information in various Emacs tools I use like [Gnus]() and [org-mode](). I chose
BBDB because it has a long standing track record and already integrates with
tons of modes I use like [Gnus](https://gnus.org.). The BBDB elisp library is
also trivial to use.

I try my best to use BBDB in a passive way where most insertions & updates are
more or less automatic, or only require my to hit `y`. I often import vCards for
new contacts I add to my email account or phone as a starting point.

BBDB has a useful feature called "auto notes" which hooks into your mail agent
and automatically annotates BBDB contacts with entries based on rules you
set. For example, you could have a rule to note the organization or newsgroup a
contact is commonly associated with.

This post describes how some of the BBDB configuration settings in my [emacs
config](.emacs) work. Keep in mind I will be updating this document as I
discover new useful entries.

_last updated: 2018-10-23_

## GitHub username

If receive notification emails from GitHub for repository activity, you can have
BBDB automatically save the username's

    (setq bbdb-auto-notes-rules
        '(("X-GitHub-Sender" (".*" github identity nil))))


## Gmail delegated inbox user

If you interact with teams using Gmail delegated inboxes, this may be useful.

If receive notification emails from GitHub for repository activity, you can have
BBDB automatically save the username's

    (setq bbdb-auto-notes-rules
        '(("X-Google-Sender-Delegation" (".*" delegate identity nil))))


## Mailer

I'm nosy and I find email applications fascinating, so I try to get the mailer
used by contacts out of interest.

    (setq bbdb-auto-notes-rules
        '(("User-Agent" (".*" mailer identity nil))
          ("X-Mailer" (".*" mailer identity nil))
          ("X-Newsreader" (".*" mailer identity nil))))

## Face

Identify either the `Face` or `X-Face` of a user. I don't show faces by default
in Gnus, but when I do toggle them, I use the Face/X-Face supplied by the
contact else I fall-back to their Gravatar using `gnus-treat-mail-gravatar`.

    (setq bbdb-auto-notes-rules
          '(("X-Face" (".+" x-face 0 'replace))
            ("Face"   (".+" face 0 'replace))))


-- Christopher Reichert
