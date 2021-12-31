{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Layout (myLayoutHook) where

import           XMonad.Layout
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BorderResize
import           XMonad.Layout.Column
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed              (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

import qualified Colors                             as C

myLayoutHook = showWName $ tallLeft ||| tallRight ||| full ||| threeCol ||| bsp ||| tab ||| one
  where
    tallLeft   = named "[]="   rt
    tallRight  = named "=[]"   $ reflectHoriz rt
    full       = named "[ ]"   $ noBorders Full
    one        = named " o "   $ Column 1.6
    threeCol   = named "|||"   $ withBorders $ ThreeColMid 1 delta (1/2)
    bsp        = named "BSP"   $ withBorders $ borderResize emptyBSP

    rt = withBorders $ ResizableTall 1 delta goldenRatio []
    delta = 3/100
    goldenRatio = 0.618033
    withBorders = smartBorders . defaultSpacing 20
    defaultSpacing i = spacingRaw True (uniformBorder i) True (uniformBorder i) True
    uniformBorder i = Border i i i i
    named t = renamed [Replace t]

    -- normal horizontal tile
    -- mtile   = named "M[]="  $ smartBorders $ Mirror rt

    -- fullscreen with tabs
    tab     = named "T"     $ noBorders $ tabbed shrinkText tabTheme

    -- two tab panes beside each other, move windows with SwapWindow
    -- tabB    = noBorders     $ tabbed shrinkText tabTheme
    -- tabtile = named "TT"    $ combineTwoP (TwoPane 0.03 0.5)
    --                                       (tabB)
    --                                       (tabB)
    --                                       (ClassName "Firefox" `Or` ClassName "Google-chrome")
    -- two layouts for gimp, tabs and tiling
    -- gimp    = named "gimp"  $ combineTwoP (TwoPane 0.03 0.15)
    --                                       (tabB) (reflectHoriz
    --                                               $ combineTwoP (TwoPane 0.03 0.2)
    --                                                 tabB        (tabB ||| Grid)
    --                                                             (Role "gimp-dock")
    --                                              )
    --                                       (Role "gimp-toolbox")

tabTheme = def
    { activeColor           = C.bg4
    , activeBorderColor     = C.bg0
    , activeTextColor       = C.fg0

    , inactiveColor         = C.bg1
    , inactiveBorderColor   = C.bg0
    , inactiveTextColor     = C.fg1

    , urgentColor           = C.red
    , urgentBorderColor     = C.bg1
    , urgentTextColor       = C.bg0

    , decoHeight            = 22
    , fontName              = "xft:Liberation Sans:size=10"
    }
