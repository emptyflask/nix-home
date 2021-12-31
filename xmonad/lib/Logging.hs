module Logging (xmobarLogHook) where

import qualified Colors                      as C
import           Control.Monad
import           System.IO
import           Text.Read                   (readMaybe)
import           XMonad
import           XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import           XMonad.Util.NamedWindows    (getName)

xmobarLogHook :: Handle -> X ()
xmobarLogHook output = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $
       def { ppOutput          = hPutStrLn output
           , ppCurrent         = fg C.greenB  .  bWrap "[" "]" -- Current workspace in xmobar
           , ppVisible         = fg C.blueB   .  bWrap " " " " -- Visible but not current workspace
           , ppHidden          = fg C.blueB   .  bWrap " " " " -- Hidden workspaces in xmobar
           , ppHiddenNoWindows = fg C.gray    .  bWrap " " " " -- Hidden workspaces
           , ppTitle           = fg C.fg4     .  shorten 80    -- Title of active window in xmobar
           , ppUrgent          = fg C.yellowB .  bWrap "<" ">" -- Urgent workspace
           , ppSep             = fg C.bg3   " | "              -- Separators in xmobar
           , ppWsSep           = ""                            -- Separators between workspaces
           , ppOrder           = \(ws:_layout:title:extras) -> [ws]++extras++[title]
           }
    where
      fg c = xmobarColor c ""
      bWrap s e = wrap (s ++ "<fn=2>") ("</fn>" ++ e)
