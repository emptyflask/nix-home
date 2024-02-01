module Managers (myManageHook, scratchpads) where

import qualified Paths
import           XMonad
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad


scratchpads :: [NamedScratchpad]
scratchpads = [ NS "calc" spawnCalc findCalc manageCalc
              , NS "htop" spawnHtop findHtop manageHtop
              , NS "obsidian" spawnObsidian findObsidian manageObsidian
              , NS "zeal" spawnZeal findZeal manageZeal
              ]
    where
      spawnCalc  = Paths.qalculate
      findCalc   = className =? "Qalculate-gtk"
      manageCalc = doCenterFloat

      spawnHtop  = Paths.kitty <> " --class=htop " <> Paths.htop
      findHtop   = className =? "htop"
      manageHtop = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

      spawnObsidian  = Paths.obsidian
      findObsidian   = className =? "obsidian"
      manageObsidian = customFloating $ W.RationalRect (1/6) (1/12) (2/3) (5/6)

      spawnZeal  = Paths.zeal
      findZeal   = className =? "Zeal"
      manageZeal = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)


myManageHook :: ManageHook
myManageHook = composeAll $
  [ manageDocks
  , manageSpawn
  , manageHook def
  , namedScratchpadManageHook scratchpads
  , isDialog --> doCenterFloat
  , isFullscreen --> doFullFloat
  , fmap not isDialog --> doF avoidMaster
  ] ++ [ matchAny v --> a | (v,a) <- myActions ]

    where

      {-|
        Script to easily find WM_CLASS for adding applications to the list
        #!/bin/sh
        exec xprop -notype \
          -f WM_NAME        8s ':\n  title =\? $0\n' \
          -f WM_CLASS       8s ':\n  appName =\? $0\n  className =\? $1\n' \
          -f WM_WINDOW_ROLE 8s ':\n  stringProperty "WM_WINDOW_ROLE" =\? $0\n' \
          WM_NAME WM_CLASS WM_WINDOW_ROLE \
          ${1+"$@"}
      -}

      myActions = [ (".kazam-wrapped"                 , doFloat)
                  , ("MPlayer"                        , doFloat)
                  , ("Oracle VM VirtualBox Manager"   , doShift "8")
                  , ("VirtualBox"                     , doShift "8")
                  , ("Wrapper-2.0"                    , doFloat)
                  , ("Xfce4-appfinder"                , doFloat)
                  , ("Xfce4-notifyd"                  , doIgnore)
                  , ("Xfrun4"                         , doFloat)
                  , ("animation-SpriteTestWindow"     , doFloat)
                  , ("animation-playbac"              , doFloat)
                  , ("file-jpeg"                      , doFloat)
                  , ("gimp-brightness-contrast-tool"  , doFloat)
                  , ("gimp-color-balance-tool"        , doFloat)
                  , ("gimp-color-selector"            , doFloat)
                  , ("gimp-colorize-tool"             , doFloat)
                  , ("gimp-curves-tool"               , doFloat)
                  , ("gimp-desaturate-tool"           , doFloat)
                  , ("gimp-display-filters"           , doFloat)
                  , ("gimp-dock"                      , ask >>= doF . W.sink)
                  , ("gimp-file-open"                 , doFloat)
                  , ("gimp-file-open-location"        , doFloat)
                  , ("gimp-file-save"                 , doFloat)
                  , ("gimp-hue-saturation-tool"       , doFloat)
                  , ("gimp-image-new"                 , doFloat)
                  , ("gimp-image-window"              , ask >>= doF . W.sink)
                  , ("gimp-keyboard-shortcuts-dialog" , doFloat)
                  , ("gimp-layer-new"                 , doFloat)
                  , ("gimp-levels-tool"               , doFloat)
                  , ("gimp-message-dialog"            , doFloat)
                  , ("gimp-modules"                   , doFloat)
                  , ("gimp-open-location"             , doFloat)
                  , ("gimp-perspective-tool"          , doFloat)
                  , ("gimp-posterize-tool"            , doFloat)
                  , ("gimp-rotate-tool"               , doFloat)
                  , ("gimp-scale-tool"                , doFloat)
                  , ("gimp-shear-tool"                , doFloat)
                  , ("gimp-threshold-tool"            , doFloat)
                  , ("gimp-tip-of-the-day"            , doFloat)
                  , ("gimp-toolbox"                   , ask >>= doF . W.sink)
                  , ("gimp-toolbox-color-dialog"      , doFloat)
                  , ("gimp-vectors-edit"              , doFloat)
                  , ("kazam"                          , doFloat)
                  , ("mpv"                            , doFloat)
                  , ("plugin-browser"                 , doFloat)
                  , ("preferences"                    , doFloat)
                  , ("procedure-browser"              , doFloat)
                  , ("rofi"                           , doIgnore)
                  , ("screenshot"                     , doFloat)
                  , ("unit-editor"                    , doFloat)
                  , ("yad-calendar"                   , doFloat)
                  , ("zoom_linux_float_video_window"  , doIgnore)
                  ]

-- Match a string against any one of a window's class, title, name or role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [appName, className, title, role]
  where
    role :: Query String
    role = stringProperty "WM_WINDOW_ROLE"


-- Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    -- W.Stack t [] (r:rs) -> W.Stack t [r] rs
    W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
    _other              -> c
