import           XMonad

import           XMonad.Config.Desktop          (desktopConfig,
                                                 desktopLayoutModifiers)

import           XMonad.Actions.GroupNavigation (historyHook)
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.ShowText        (handleTimerEvent)

import           XMonad.Hooks.EwmhDesktops      (ewmh, ewmhFullscreen)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook       (NoUrgencyHook (..),
                                                 withUrgencyHook)

import           XMonad.Util.Run                (spawnPipe)
import           XMonad.Util.SpawnOnce          (spawnOnOnce)

import qualified Colors
import           Keys                           (myKeys)
import           Layout                         (myLayoutHook)
import           Logging                        (xmobarLogHook)
import           Managers                       (myManageHook)
import qualified Paths
import qualified Workspaces

main :: IO ()
main = do
  xmproc <- spawnPipe Paths.xmobar
  xmonad $ withUrgencyHook NoUrgencyHook
      . ewmh
      . ewmhFullscreen
      . withNavigation2DConfig def
        { defaultTiledNavigation = hybridOf sideNavigation centerNavigation }

      $ desktopConfig
        { borderWidth        = 2
        , focusedBorderColor = Colors.green
        , normalBorderColor  = Colors.bg0

        , clickJustFocuses   = False
        , clientMask         = clientMask desktopConfig
        , focusFollowsMouse  = False

        , handleEventHook    = handleEventHook desktopConfig
                                <+> handleTimerEvent

        , handleExtraArgs    = handleExtraArgs desktopConfig
        , keys               = myKeys
        , layoutHook         = desktopLayoutModifiers myLayoutHook
        , logHook            = logHook desktopConfig
                                <+> historyHook
                                <+> xmobarLogHook xmproc
        , manageHook         = manageHook desktopConfig
                                <+> myManageHook

        , modMask            = mod4Mask
        , mouseBindings      = mouseBindings desktopConfig
        , rootMask           = rootMask desktopConfig
        , startupHook        = startup
        , terminal           = Paths.kitty
        , workspaces         = Workspaces.numbered
        }

  where
    startup = do
      startupHook desktopConfig
      spawnOnOnce "2" "thunderbird"
      spawnOnOnce "3" "slack"
      spawnOnOnce "3" "signal-desktop"
      spawnOnOnce "9" "spotify"
      setWMName "LG3D" -- Java app focus fix
