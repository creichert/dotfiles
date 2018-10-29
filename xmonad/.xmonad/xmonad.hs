
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
import XMonad.Util.SpawnOnce


main :: IO ()
main = do
    h <- spawnPipe "xmobar"
    xmonad $ docks $ def {
               terminal    = "xterm"
             , borderWidth = 1
             , keys        = keybindings def
             , layoutHook  = smartBorders $ avoidStruts $ layoutHook def
             , manageHook  = composeAll [
                               manageDocks
                             , namedScratchpadManageHook scratchpads
                             , className =? "Chromium" --> doF (W.shift "8")
                             , className =? "Spotify"  --> doF (W.shift "9")
                             , manageHook def
                             ]
             , logHook = dynamicLogWithPP $ xpp h
             , handleEventHook = mconcat [ docksEventHook, handleEventHook def ]
             , modMask = mod4Mask
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
    xpCfg = xPCfg { font = "xft:monofur:size=10:style=italic" }
    keys' XConfig { XMonad.modMask = modm } = Map.fromList [

          ((modm, xK_F1), spawn "xterm")
        , ((0, xF86XK_Launch1), spawn "xterm")

        -- dmenu w/ history (see bin/dmenu_run_history)
        -- - C-n : next in history
        -- - C-p : prev in history
        , ((modm, xK_p), spawn "dmenu_run_history -nb '#3b3228' -nf '#d0c8c6' -fn 'monofur-11'")

        -- audio (amixer & mpris)
        , ((0, 0x1008ff12), spawn "amixer set Master toggle")
        , ((modm, 0x1008ff12), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

        , ((0, 0x1008ff11), spawn "amixer -c 0 set 'Master,0' 5%- > /dev/null")
        , ((0, 0x1008ff13), spawn "amixer -c 0 set Master 5%+ unmute > /dev/null")
        , ((modm, 0x1008ff11), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ((modm, 0x1008ff13), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

        , ((modm .|. shiftMask, xK_F1), spawn "emacs -fs")
        , ((modm .|. shiftMask, xK_F2), spawn "emacsclient -c")

        , ((modm, xK_apostrophe),      passPrompt xpCfg)
        , ((modm, xK_s),               sshPrompt xpCfg)
        , ((modm .|. shiftMask, xK_m), manPrompt xpCfg)

        , ((modm, xK_r),  namedScratchpadAction scratchpads "psql")
        , ((modm, xK_k),  namedScratchpadAction scratchpads "terminal")
        , ((modm, xK_j),  namedScratchpadAction scratchpads "emacs")
        , ((modm, xK_o),  namedScratchpadAction scratchpads "spotify")
        , ((modm, xK_m),  namedScratchpadAction scratchpads "gnus")
        , ((modm, xK_g),  namedScratchpadAction scratchpads "ghci")

          -- Take a selective screenshot using the command specified by mySelectScreenshot.
        , ((modm .|. shiftMask, xK_p), spawn screenshotRegion)
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
          (customFloating $ W.RationalRect (1/10) (1/6) (12/15) (2/3))

    , NS "emacs" "emacs --title org" (title =? "org")
          (customFloating $ W.RationalRect (1/8) (1/8) (7/9) (4/5))

    , NS "ghci" "xterm -title ghci -e stack exec ghci" (title =? "ghci")
          (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

    , NS "psql" "emacs -f sql-postgres --title psql" (title =? "psql")
          (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

    , NS "gnus" "emacs -f gnus --title mail" (title =? "mail")
          (customFloating $ W.RationalRect (1/20) (1/20) (17/20) (17/20))

    , NS "spotify" "spotify" (className =? "Spotify")
          (customFloating $ W.RationalRect (1/20) (1/20) (17/20) (17/20))
    ]
