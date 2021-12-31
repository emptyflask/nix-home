module Keys (myKeys) where

import qualified Data.Map                           as M
import           XMonad
import qualified XMonad.StackSet                    as W

import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.GridSelect          as GS
import qualified XMonad.Actions.GroupNavigation     as GN
import qualified XMonad.Actions.Navigation2D        as N2D
import qualified XMonad.Actions.ShowText            as T
import           XMonad.Actions.WithAll             (sinkAll)

import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.ResizableTile        (MirrorResize (..))
import           XMonad.Layout.Spacing

import qualified XMonad.Prompt                      as Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Ssh

import           XMonad.Util.NamedScratchpad        (namedScratchpadAction)
import           XMonad.Util.Run                    (runInTerm)

import           Graphics.X11.ExtraTypes.XF86
import           System.Exit

import           Managers                           (scratchpads)

-- Keyboard --
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@ XConfig {XMonad.modMask = modm} = M.fromList $
    -- launching and killing programs
    [ ((modm,                     xK_Return   ), spawn "kitty")
    , ((modm .|. alt,             xK_Return   ), spawn "urxvt")
    , ((modm,                     xK_f        ), spawn "thunar")
    , ((modm .|. shift,           xK_c        ), kill)
    , ((modm .|. shift .|. ctrl,  xK_c        ), spawn "xkill")

    -- layouts
    , ((modm,                xK_b        ), sendMessage ToggleStruts)
    , ((modm,                xK_equal    ), decScreenWindowSpacing 2)
    , ((modm .|. shift,      xK_equal    ), setScreenWindowSpacing 0)
    , ((modm,                xK_minus    ), incScreenWindowSpacing 2)
    , ((modm .|. shift,      xK_minus    ), setScreenWindowSpacing 20)
    , ((modm,                xK_space    ), sendMessage NextLayout)
    , ((modm .|. shift,      xK_space    ), flash "Reset Layout" >> setLayout (XMonad.layoutHook conf))

    -- floating layer stuff
    , ((modm,                xK_t        ), withFocused $ windows . W.sink)
    , ((modm .|. shift,      xK_t        ), sinkAll)

    -- refresh
    , ((modm,                xK_F5       ), flash "Refresh" >> refresh)

    -- focus
    , ((alt,            xK_Tab      ), GN.nextMatch GN.History (return True))
    , ((modm,           xK_Tab      ), moveTo Next NonEmptyWS)
    , ((modm .|. shift, xK_Tab      ), moveTo Prev NonEmptyWS)
    , ((modm,           xK_j        ), windows W.focusDown)
    , ((modm,           xK_k        ), windows W.focusUp)
    , ((modm,           xK_Down     ), windows W.focusDown)
    , ((modm,           xK_Up       ), windows W.focusUp)
    , ((modm,           xK_m        ), windows W.focusMaster)

    , ((modm,           xK_Right        ), moveTo Next realWorkspace)
    , ((modm,           xK_Left         ), moveTo Prev realWorkspace)
    , ((modm,           xK_bracketright ), moveTo Next realWorkspace)
    , ((modm,           xK_bracketleft  ), moveTo Prev realWorkspace)
    , ((modm .|. shift, xK_Right        ), shiftTo Next realWorkspace)
    , ((modm .|. shift, xK_Left         ), shiftTo Prev realWorkspace)
    , ((modm .|. shift, xK_bracketright ), shiftTo Next realWorkspace)
    , ((modm .|. shift, xK_bracketleft  ), shiftTo Prev realWorkspace)

    -- Directional navigation of windows
    , ((modm .|. alt,        xK_Right    ), N2D.windowGo R False)
    , ((modm .|. alt,        xK_Left     ), N2D.windowGo L False)
    , ((modm .|. alt,        xK_Up       ), N2D.windowGo U False)
    , ((modm .|. alt,        xK_Down     ), N2D.windowGo D False)

    -- Binary Space Partition
    , ((modm .|. alt,               xK_l     ), sendMessage $ ExpandTowards R)
    , ((modm .|. alt,               xK_h     ), sendMessage $ ExpandTowards L)
    , ((modm .|. alt,               xK_j     ), sendMessage $ ExpandTowards D)
    , ((modm .|. alt,               xK_k     ), sendMessage $ ExpandTowards U)
    , ((modm .|. alt .|. ctrl,      xK_l     ), sendMessage $ ShrinkFrom R)
    , ((modm .|. alt .|. ctrl,      xK_h     ), sendMessage $ ShrinkFrom L)
    , ((modm .|. alt .|. ctrl,      xK_j     ), sendMessage $ ShrinkFrom D)
    , ((modm .|. alt .|. ctrl,      xK_k     ), sendMessage $ ShrinkFrom U)
    , ((modm,                       xK_r     ), sendMessage Rotate)
    , ((modm,                       xK_s     ), sendMessage Swap)
    , ((modm,                       xK_n     ), sendMessage FocusParent)
    , ((modm .|. ctrl,              xK_n     ), sendMessage SelectNode)
    , ((modm .|. shift,             xK_n     ), sendMessage MoveNode)
    , ((modm,                       xK_a     ), sendMessage Balance)
    , ((modm .|. shift,             xK_a     ), sendMessage Equalize)

    -- swapping
    , ((modm .|. shift,      xK_Return   ), windows W.swapMaster)
    , ((modm .|. shift,      xK_j        ), windows W.swapDown)
    , ((modm .|. shift,      xK_k        ), windows W.swapUp)
    -- , ((modm,                xK_s        ), sendMessage $ SwapWindow)

    -- increase or decrease number of windows in the master area
    , ((modm,                xK_comma    ), sendMessage (IncMasterN 1))
    , ((modm,                xK_period   ), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modm,                xK_h        ), sendMessage Shrink)
    , ((modm,                xK_l        ), sendMessage Expand)
    , ((modm .|. shift,      xK_h        ), sendMessage MirrorShrink)
    , ((modm .|. shift,      xK_l        ), sendMessage MirrorExpand)

    -- grid select
    , ((modm,                xK_g        ), GS.goToSelected GS.def)

    -- search (modm-s [g,h,w,y])
    -- , ((modm,                xK_s        ), promptSearch)
    -- , ((modm .|. shift,      xK_s        ), selectSearch)

    -- quit, or restart
    -- , ((modm .|. shift,      xK_q        ), spawn "xfce4-session-logout")
    , ((modm .|. shift,      xK_q        ), io exitSuccess) -- %! Quit xmonad
    , ((modm .|. ctrl,       xK_q        ), spawn "systemctl suspend")
    -- , ((modm .|. alt,        xK_l        ), spawn "xautolock -locknow")

    -- Restart xmonad
    , ((modm,                xK_q        ), restart "xmonad" True)

    , ((modm,                xK_p        ), spawn "systemctl --user restart picom.service")

    -- ungrab mouse cursor from applications which can grab it
    , ((modm,                xK_i        ), spawn "xdotool key XF86Ungrab")

    -- open man pages / ssh consoles
    , ((modm,                xK_F1       ), manPrompt Prompt.def)
    , ((modm,                xK_F2       ), sshPrompt Prompt.def)

    -- rofi
    , ((alt,                 xK_space    ), spawn "rofi -combi-modi window,drun -modi combi -show combi")
    , ((modm,                xK_o        ), spawn "rofi -modi run -show run")
    -- , ((ctrl .|. alt,        xK_c        ), spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -theme oxide -width 900 -lines 15")
    , ((modm .|. alt,        xK_c        ), spawn "CM_LAUNCHER=rofi clipmenu")

    -- scratchpads
    , ((modm,                xK_c        ), namedScratchpadAction scratchpads "calc")
    , ((modm,                xK_grave    ), namedScratchpadAction scratchpads "htop")
    , ((modm,                xK_z        ), namedScratchpadAction scratchpads "zeal")

    , ((modm,                xK_v        ), runInTerm "" "alsamixer -c 0")

    -- screenshot tool
    , ((noModMask,           xK_Print ), spawn "flameshot gui")

    ] ++ workspaceKeys ++ mediaKeys

 where
    ctrl  = controlMask
    alt   = mod1Mask
    shift = shiftMask

    -- promptSearch :: X ()
    -- promptSearch = do flash "Search ?"
    --                   Submap.submap . searchEngineMap $ Search.promptSearch Prompt.def

    -- selectSearch :: X ()
    -- selectSearch = do flash "Select Search ?"
    --                   Submap.submap . searchEngineMap $ Search.selectSearch

    -- searchEngineMap :: (Search.SearchEngine -> X ()) -> M.Map (KeyMask, KeySym) (X ())
    -- searchEngineMap method = M.fromList
    --   [ ((0, xK_d), method Search.dictionary)
    --   , ((0, xK_g), method Search.google)
    --   , ((0, xK_h), method Search.hoogle)
    --   , ((0, xK_i), method Search.images)
    --   , ((0, xK_m), method Search.maps)
    --   , ((0, xK_w), method Search.wikipedia)
    --   , ((0, xK_y), method Search.youtube)
    --   ]

    flash :: String -> X ()
    flash text =
      T.flashText def 1 (" " ++ text ++ " ")

    -- mod-[1..9]       Switch to workspace N
    -- mod-shift-[1..9] Move client to workspace N
    workspaceKeys :: [((KeyMask, KeySym), X ())]
    workspaceKeys =
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (cycle wkSpaces) $ numKeys ++ numpadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
        where
          wkSpaces   = XMonad.workspaces conf
          numKeys    = [xK_1 .. xK_9]
          numpadKeys = [xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down,
                        xK_KP_Left, xK_KP_Begin, xK_KP_Right,
                        xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up]

    -- -- mod-{w,e,r}       Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r} Move client to screen 1, 2, or 3
    -- screenKeys :: [((KeyMask, KeySym), X ())]
    -- screenKeys =
    --   [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
    --     , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
    --   ]

    mediaKeys :: [((KeyMask, KeySym), X ())]
    mediaKeys =
      [ ((0 , xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
      , ((0 , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
      , ((0 , xF86XK_AudioMute),        spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")
      ]

    realWorkspace :: WSType
    realWorkspace = WSIs hiddenNotNSP

    hiddenNotNSP :: X (WindowSpace -> Bool)
    hiddenNotNSP = do
      hs <- gets $ map W.tag . W.hidden . windowset
      return (\w -> (W.tag w) /= "NSP" && (W.tag w) `elem` hs)
