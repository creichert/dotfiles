
-- | creichert xmonad config

import qualified Data.List as L
import qualified Data.Map  as Map

import Data.Map                     (Map)
import Graphics.X11.ExtraTypes.XF86
import System.IO

-- XMonad core
import XMonad
import XMonad.StackSet as W

-- XMonad contrib
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run             (spawnPipe)


-- standard colors and fonts that match the current theme.
backgroundColor, foregroundColor, color1, themeFont :: String
backgroundColor = "#3b3228"
foregroundColor = "#d0c8c6"
color1 = "#cbc6077"
themeFont = "monofur"

main :: IO ()
main = do

    h <- spawnPipe $ "xmobar "
                   ++ " -f 'xft:" ++ themeFont ++ ":size=9:antialias=true'"
                   ++ " -B '" ++ backgroundColor ++ "'"
                   ++ " -F '" ++ foregroundColor ++ "'"
                   ++ " -p Top"

    xmonad $ docks $ def {
               terminal    = "xterm"
               -- "Windows" key is used for key combinations to avoid
               -- collisions w/ emacs
             , modMask = mod4Mask
             , borderWidth = 1
             , normalBorderColor = foregroundColor
             , focusedBorderColor = color1
             , keys        = keybindings def
             , layoutHook  = smartBorders $ avoidStruts $ layoutHook def
             , manageHook  = composeAll [
                               manageDocks
                             , namedScratchpadManageHook scratchpads
                             -- these windows always get pushed to the same
                             -- workspace on startup
                             , className =? "chromium" --> doF (W.shift "8")
                             , className =? "spotify"  --> doF (W.shift "9")
                             , manageHook def
                             ]
             , logHook = dynamicLogWithPP $ xpp h
             , handleEventHook = mconcat [ docksEventHook, handleEventHook def ]
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

        -- dmenu w/ history (see bin/dmenu_run_history)
        -- - C-n : next in history
        -- - C-p : prev in history
        , ((modm, xK_p), spawn $ "dmenu_run_history"
                               ++ " -nb \"" ++ backgroundColor ++ "\""
                               ++ " -nf \"" ++ foregroundColor ++ "\""
                               ++ " -fn \"" ++ themeFont ++ "-11\"")

        -- audio (amixer & mpris)
        , ((0, 0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ((0, 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ((0, 0x1008ff13), spawn "pactl set-sink-mute @DEFAULT_SINK@ 0 && pactl set-sink-volume @DEFAULT_SINK@ +5%")

        -- dbus / mpris control over playback
        , ((modm, 0x1008ff12), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ((modm, 0x1008ff11), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ((modm, 0x1008ff13), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

        , ((modm .|. shiftMask, xK_F1), spawn "emacs -fs")
        , ((modm .|. shiftMask, xK_F2), spawn "emacsclient -c")

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

    , NS "agenda" "emacs --title agenda" (title =? "agenda")
          (customFloating $ W.RationalRect (1/20) (1/20) (18/20) (18/20))

    , NS "ghci" "xterm -title ghci -e stack exec ghci" (title =? "ghci")
          (customFloating $ W.RationalRect (1/20) (1/10) (17/20) (7/10))

    , NS "psql" "emacs -f sql-postgres --title psql" (title =? "psql")
          (customFloating $ W.RationalRect (1/20) (1/20) (17/20) (17/20))
    ]
