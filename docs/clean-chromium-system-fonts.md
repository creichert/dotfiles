---
title: Clean chromium system fonts
author: creichert
date: 2018-09-23
layout: post
---

For a lot of websites, I prefer to override the default css to use my system
fonts. I've tried several browser extensions to accomplish this goal, but most
of them suffered from the same problem: **they override a global wildcard css
selector and mark it as `!important`**. This can cause a lot of icons and other
css glyphs to show incorrectly (usually as a box).

## gtk

GTK doesn't honor Xresources (as far as I know). These gtk settings will
configure applications to use your system fonts.

- `~/.gtkrc-2.0`

    ```
    style "user-font"
    {
            font_name="monofur 12"
    }

    widget_class "*" style "user-font"

    gtk-font-name="monofur 12"
    monospace-font-name="monofur 12"
    ```

- `~/.config/gtkrc-3.0/settings.ini`

    ```
    [Settings]
    gtk-font-name = monofur 12

    ```

## stylebot

Chromium is a bit more difficult. The GTK settings will set the font for most of
chrome/chromium **outside of the webpage view**.

However, I also like to configure many websites to use my system fonts as
well. Chrome does have a built-in font configuration dialogue, but it doesn't
override sites that set their own css fonts.

To override the default font and allow white-listing some sites, I use the
[stylebot
extension](https://chrome.google.com/webstore/detail/stylebot/oiaejidbmkiecgbjeifoejpgmdaleoha?hl=en).

In the stylebot css configuration, I use the following snippet.

```css
body, p, td, h1, h2, h3, h4, h5, h6, tt, code, pre, a, cite, li {
    font-family: "monofur", monospace, sans-serif;
}
```

The good thing about this configuration is that it doesn't override _all_ styles
with a wildcard selector. I've only tested it for a few days, but it seems to
work perfectly on every site I've tried.

like some other extensions which "force" user fonts do. The big problem with
forcing system fonts using the `*` wildcard css selector is icon sets like
`font-awesome` do not work

## cvim

I also use cvim which, alas, has it's _own_ css stetings 😧. For the cvim
extension, I use [this
gist](https://gist.github.com/creichert/31914a6dc517b22f4a21777c463ab4ff) to
configure cvim and keep the settings synced. The `cvim.css` replaces the color
and font to that of my xresources config.
