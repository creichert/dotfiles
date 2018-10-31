---
title: Modern Haskell style guide
author: creichert
layout: post
---

I dislike computer mice. In the past, I've had various problems w/ mice; the
worst of which was hand/wrist fatigue which was contributing significantly to
RSI & carpal tunnel symptoms.

Recently, I bought a Logitech M570 to try out a trackball mouse. I've loved
using a trackball and would recommend one to anyone who does the vast majority
of their work using the keyboard

One problem I hit early on was I wanted to scroll using the trackball and my
thumb. Repeatedly scrolling using the wheel is slow and wears out my wrists very
quickly

To solve this problem, I customized my X11 xinput settings so that my Right
Click is the scroll method button. 

    # Hold down right click to use trackball to scroll
    xinput set-prop 8 "libinput Scroll Method Enabled" 0, 0, 1
    xinput set-prop 8 "libinput Button Scrolling Button" 3
    
Now, I can hold down the right click button and use the trackball to scoll
vertically and horizontally.

- creichert
