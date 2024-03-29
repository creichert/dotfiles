
-- | creichert xmonad config

import qualified Data.List as L
import qualified Data.Map  as Map

import Data.Map                     (Map)
import Graphics.X11.ExtraTypes.XF86
import System.IO

-- XMonad core
import XMonad
import XMonad.StackSet as W hiding (focus)

-- XMonad contrib
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run              (spawnPipe)


-- standard colors and fonts that match the current theme.
backgroundColor, foregroundColor, color1, color12, color15, themeFont :: String
backgroundColor = "#3b3228"
foregroundColor = "#d0c8c6"
color1 = "#cbc6077"
color12 = "#8ab3b5"
color15 = "#f5eeeb"
themeFont = "monofur"

main :: IO ()
main = do

    -- Although these are initially setup in .xprofile, they are called again
    -- here as it's often useful to simply recompile xmonad if the keyboard or
    -- mouse are disconnected.
    spawn "xset r rate 250 60"
    spawn "setxkbmap -option ctrl:nocaps"
    h <- spawnPipe $ "xmobar "
                   ++ " -f 'xft:" ++ themeFont ++ ":size=9:antialias=true'"
                   ++ " -B '" ++ backgroundColor ++ "'"
                   ++ " -F '" ++ foregroundColor ++ "'"
                   ++ " -p Top"

    xmonad $ ewmh $ docks $ def {
               terminal    = "xterm"
               -- "Windows" key is used for key combinations to avoid
               -- collisions w/ emacs
             , modMask = mod4Mask
             , borderWidth = 1
             , normalBorderColor = foregroundColor
             , focusedBorderColor = color1
             , keys = keybindings def
             , mouseBindings = mousebindings def
             , layoutHook  = smartBorders $ avoidStruts $ layoutHook def
             , manageHook  = composeAll [
                               className =? "Dunst" --> doIgnore
                             , namedScratchpadManageHook scratchpads
                             -- these windows always get pushed to the same
                             -- workspace on startup
                             , className =? "Chromium" --> doF (W.shift "8")
                             , title =? "Save File" --> doCenterFloat
                             , title =? "Open File" --> doCenterFloat
                             , title =? "Open Files" --> doCenterFloat
                             , manageHook def
                             ]
             , logHook = dynamicLogWithPP $ xpp h
             , handleEventHook = mconcat [
                       -- Spotify does not set the WM_CLASS until after it's
                       -- already loaded. By that point X11/XMonad have already
                       -- processed the window. This dynamic property shifts the
                       -- window when WM_CLASS is set.
                       dynamicPropertyChange "WM_CLASS" (className =? "Spotify" --> doF (W.shift "9"))
                     , handleEventHook def
                     , fullscreenEventHook
                     ]
             }
  where
    xpp h = xmobarPP {
              ppOutput = hPutStrLn h
            , ppExtras = [
                  padL loadAvg
                , logCmd "tail ~/.xmonad/xmonad.errors"
                ]
            }

-- Merge my keybindings w/ the default keybindings.
keybindings :: XPConfig -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
keybindings xPCfg x = keys' x `Map.union` keys def x
  where
    xpCfg = xPCfg { font = "xft:" ++ themeFont ++ ":size=10" }
    keys' XConfig { XMonad.modMask = modm } = Map.fromList [

          ((modm, xK_F1), spawn "xterm")
        , ((0, xF86XK_Launch1), spawn "xterm")

        , ((modm .|. shiftMask, xK_z), spawn
              "systemctl --user start xscreensaver.service && xscreensaver-command -activate")

        , ((modm, xK_p), spawn $ "dmenu_run_history"
                               ++ " -nb \"" ++ backgroundColor ++ "\""
                               ++ " -nf \"" ++ foregroundColor ++ "\""
                               ++ " -sf \"" ++ color15 ++ "\""
                               ++ " -sb \"" ++ color12 ++ "\""
                               ++ " -fn \"" ++ themeFont ++ "-11\"")

        -- audio (amixer & mpris)
        , ((0, 0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ((0, 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ((0, 0x1008ff13), spawn "pactl set-sink-mute @DEFAULT_SINK@ 0 && pactl set-sink-volume @DEFAULT_SINK@ +5%")
        -- XF86AudioPlay
        , ((0, 0x1008ff14), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ((0, 0x1008ff16), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ((0, 0x1008ff17), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

        , ((modm .|. shiftMask, xK_F1), spawn "emacs -fs")
        , ((modm .|. shiftMask, xK_F2), spawn "emacsclient -c")
        , ((modm, xK_F9), spawn "systemctl restart --user pulseaudio.service")

        , ((modm, xK_apostrophe), passPrompt xpCfg)
        , ((modm, xK_s),          sshPrompt xpCfg)
        , ((modm, xK_i),          manPrompt xpCfg)

        , ((modm, xK_m), spawn "emacs -f gnus")
        , ((modm .|. shiftMask, xK_m), spawn "emacs -f gnus-unplugged")

          -- Take a selective screenshot using the command specified by mySelectScreenshot.
        , ((modm .|. shiftMask, xK_p), spawn screenshotRegion)

        , ((modm, xK_r),  namedScratchpadAction scratchpads "psql")
        , ((modm, xK_k),  namedScratchpadAction scratchpads "terminal")
        , ((modm, xK_j),  namedScratchpadAction scratchpads "agenda")
        , ((modm, xK_g),  namedScratchpadAction scratchpads "ghci")

        , ((modm .|. shiftMask, xK_period), spawn "dunstctl history-pop")
        , ((modm .|. shiftMask, xK_space), spawn "dunstctl close")
        ]

-- | Mouse bindings: default actions bound to mouse events
mousebindings :: XPConfig -> XConfig Layout -> Map (KeyMask, Button) (Window -> X ())
mousebindings _xPCfg x = mouse' x `Map.union` mouseBindings def x
  where
    mouse' XConfig { XMonad.modMask = modm } = Map.fromList [

        ((modm .|. shiftMask, button1), (\w ->
            focus w >> mouseResizeWindow w
                    >> windows W.shiftMaster))
      ]

screenshotRegion :: String
screenshotRegion = L.intercalate ";" [
      "sleep 0.7"
    , "scrot '/tmp/%Y-%m-%d_$wx$h_screenshot.png' --quality 100 --select -e '" ++ clipAndCp ++ "'"
    ]
  where
    clipAndCp = "xclip -selection primary -selection clipboard -t image/png -i $f; cp $f ~/downloads/screenshots/latest.png; mv $f ~/downloads/screenshots"

scratchpads :: [NamedScratchpad]
scratchpads = [
      NS "terminal" "xterm -T terminal" (title =? "terminal")
          (customFloating $ W.RationalRect (1/20) (1/10) (17/20) (7/10))

    , NS "agenda" "emacs -f org-agenda --title agenda" (title =? "agenda")
          (customFloating $ W.RationalRect (1/20) (1/20) (18/20) (18/20))

    , NS "ghci" "xterm -title ghci -e stack exec ghci" (title =? "ghci")
          (customFloating $ W.RationalRect (1/20) (1/10) (17/20) (7/10))

    , NS "psql" "emacs -f sql-postgres --title psql" (title =? "psql")
          (customFloating $ W.RationalRect (1/20) (1/20) (17/20) (17/20))
    ]
