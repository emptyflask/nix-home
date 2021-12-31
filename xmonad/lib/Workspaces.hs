module Workspaces where

import qualified Data.Map.Strict as M
import           Data.Maybe

data Workspace = Workspace
    { name  :: String
    , glyph :: String
    }
    deriving (Show)

workspaces :: [Workspace]
workspaces =
  [ Workspace "Main" "\xf120"
  , Workspace "Mail" "\xf0e0"
  , Workspace "Chat" "\xf075"
  , Workspace "Work-1" "\xf3a5"
  , Workspace "Work-2" "\x03bb"
  , Workspace "Work-3" "\xf03e"
  , Workspace "Work-4" "\xf1b2"
  , Workspace "Games" "\xf11b"
  , Workspace "Music" "\xf025"
  ]

numbered :: [String]
numbered = map show [1 .. (length workspaces)]

indexedWorkspaces :: M.Map Int Workspace
indexedWorkspaces = withIndex workspaces

-- ["a", "b"] -> Map.fromList [(1,"a"), (2,"b")]
withIndex :: (Ord k, Num k, Enum k) => [a] -> M.Map k a
withIndex = M.fromList . zip [1 ..]

lookup :: Int -> Maybe Workspace
lookup n = M.lookup n $ withIndex workspaces

polybarString :: Int -> Int -> String
polybarString currWs ws
  | currWs == ws = "%{B#2c3e50}%{u#6e98a4} " ++ glyphFor ws ++ " %{B-}%{-u}"
  | otherwise = " " ++ glyphFor ws ++ " "
  where
    glyphFor n = fromMaybe "." $ M.lookup n $ withIndex $ glyphs workspaces

-- toString :: Int -> String
-- toString current = unwords . M.elems $ M.mapWithKey fmt indexedWorkspaces
--   where
--     fmt current ws = "%{B#2c3e50}%{u#6e98a4} " ++ glyph ws ++ " %{B-}%{-u}"
--     fmt _ ws = " " ++ glyph ws ++ " "

names :: [Workspace] -> [String]
names = map name

glyphs :: [Workspace] -> [String]
glyphs = map glyph
