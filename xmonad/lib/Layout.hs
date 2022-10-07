{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Layout (myLayoutHook) where

import           XMonad.Layout
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BorderResize
import           XMonad.Layout.Column
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed              (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

import           CenteredIfSingle
import qualified Colors

myLayoutHook = showWName $ tallLeft ||| tallRight ||| full ||| threeCol ||| bsp ||| tab ||| one ||| nCols
  where
    tallLeft   = named "[]="   $ withBorders $ ResizableTall nmaster delta goldenRatio []
    tallRight  = named "=[]"   $ reflectHoriz tallLeft
    full       = named "[ ]"   $ noBorders Full
    one        = named " o "   $ withBorders $ Column 1.6
    threeCol   = named "|||"   $ defaultSpacing 20 . centered $ ThreeColMid nmaster delta (1/2)
    bsp        = named "BSP"   $ withBorders $ borderResize emptyBSP
    nCols      = named "|n|"   $ withBorders $ multiCol [0] 4 0.01 (-0.5)

    goldenRatio      = 144/233
    nmaster          = 1       -- Default number of windows in the master pane
    delta            = 3/100   -- Percent of screen to increment by when resizing panes
    withBorders      = smartBorders . defaultSpacing 20
    defaultSpacing i = spacingRaw True (uniformBorder i) True (uniformBorder i) True
    uniformBorder i  = Border i i i i
    named t          = renamed [Replace t]
    centered         = centeredIfSingle goldenRatio 0.8

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
    { activeColor           = Colors.bg4
    , activeBorderColor     = Colors.bg0
    , activeTextColor       = Colors.fg0

    , inactiveColor         = Colors.bg1
    , inactiveBorderColor   = Colors.bg0
    , inactiveTextColor     = Colors.fg1

    , urgentColor           = Colors.red
    , urgentBorderColor     = Colors.bg1
    , urgentTextColor       = Colors.bg0

    , decoHeight            = 22
    , fontName              = "xft:Liberation Sans:size=10"
    }
