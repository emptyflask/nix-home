module Logging (xmobarLogHook) where

import qualified Colors
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

xmobarLogHook :: Handle -> X ()
xmobarLogHook output = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $
    def { ppOutput          = hPutStrLn output
        , ppCurrent         = fg Colors.greenB  .  bWrap "[" "]" -- Current workspace in xmobar
        , ppVisible         = fg Colors.blueB   .  bWrap " " " " -- Visible but not current workspace
        , ppHidden          = fg Colors.blueB   .  bWrap " " " " -- Hidden workspaces in xmobar
        , ppHiddenNoWindows = fg Colors.gray    .  bWrap " " " " -- Hidden workspaces
        , ppTitle           = fg Colors.fg4     .  shorten 80    -- Title of active window in xmobar
        , ppUrgent          = fg Colors.yellowB .  bWrap "<" ">" -- Urgent workspace
        , ppSep             = fg Colors.bg3   " | "              -- Separators in xmobar
        , ppWsSep           = ""                                 -- Separators between workspaces
        , ppOrder           = \(ws:_layout:name:extras) -> concat [[ws], extras, [name]]
        }
            where
                fg :: String -> String -> String
                fg c = xmobarColor c ""

                bWrap :: String -> String -> String -> String
                bWrap s e = wrap (s ++ "<fn=2>") ("</fn>" ++ e)
