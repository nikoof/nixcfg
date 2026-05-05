{-# LANGUAGE Haskell98 #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- Copyright (c) Miles Sabin. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its contributors
-- may be used to endorse or promote products derived from this software without
-- specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
-- USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ThreeColumnStable
-- Description :  Three column layout for Ultrawide monitors
-- Copyright   :  (c) 2025 Miles Sabin
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Miles Sabin <miles@milessabin.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A three column layout suitable for ultrawide monitors. It populates
-- starting from the centre column, then fills the right column, then
-- shifts all the windows to the left and fills the newly empty right
-- column. From this point on it inserts new windows into the same
-- column and below the window that is currently focused.
--
-- The "stable" part of the name signifies that windows will not be moved
-- between or within columns as other windows are added or removed. Moving
-- windows between columns is supported via the "MoveWindow" message and
-- whole columns can be moved left or right using the "MoveColumn" message.
-- The "RotateColumns" messages rotates all three columns left or right,
-- wrapping around at the ends. The "TidyColumns" message packs the columns
-- to the centre and right, leaving the left column and possibly the right
-- column empty if there are one or two columns currently empty. If closing
-- a window leaves a single window visible then it will be moved to the
-- centre column automatically.
--
-- This layout gets along well with "Xmonad.Actions.Navigation2D" and
-- "XMonad.Layout.PerWorkspace".
--
-- This layout began its life as a modification of
-- "XMonad.Layout.CenterMainFluid" which works well on Ultrawide monitors
-- but does have the drawback of assigning and moving windows to columns
-- unpredictably as windows are added or removed. This layout attempts to
-- avoid that in a hopefully intuitive way and gets it "Stable" suffix from
-- this behaviour.
--
-- Some inspiriation for the mechanics of this layout came from
-- "XMonad.Layout.Columns" and "XMonad.Layout.BinarySpacePartition"
-- although I think it is quite a bit simpler then either of those.
-----------------------------------------------------------------------------

module XMonad.Layout.ThreeColumnStable
  ( -- * Usage
    -- $usage
    emptyTCS,
    ThreeColumnStable (..),
    MoveWindow (..),
    MoveColumn (..),
    RotateColumns (..),
    TidyColumns (..)
  ) where

import XMonad as X hiding (moveWindow)
import qualified XMonad.StackSet as W
import XMonad.Util.Types

import Control.Monad (mfilter)
import qualified Data.Map as M
-- $usage
-- You can use this module by adding following in your @xmonad.hs@:
--
-- > import XMonad.Layout.ThreeColumnStable
--
-- Then add the layout, using the default constructor,
--
-- > myLayout = emptyTCS ||| etc ..
--
-- You might find "XMonad.Actions.Navigation2D" useful to move between the windows.
--
-- You can also use the following messages to control the layout:
--
-- "MoveColumn" moves the focused column left or right.
-- "MoveWindow" moves the focused window one column left or right.
-- "RotateColumns" rotates the columns left or right.
--
-- All of these messages take a "Direction1D" to specify the move/rotate direction
-- and a "Bool" arguments which, if "True", will wrap the window or column around to
-- the other side if it is at the edge.
--
-- "TidyColumns" packs the columns to the centre and right, leaving the left column and
-- possibly the right column empty if there are one or two columns currently empty.

type Column = [Window]
type Columns = (Column, Column, Column)

-- | Create an empty 'ThreeColumnStable' layout with the given delta and ratio
emptyTCS :: Rational -> Rational -> ThreeColumnStable a
emptyTCS delta ratio = ThreeColumnStable delta ratio ([], [], [])

data ThreeColumnStable a = ThreeColumnStable Rational Rational Columns
  deriving (Show,Read)

data MoveWindow = MoveWindow Direction1D Bool deriving (Show, Read)
instance Message MoveWindow

data MoveColumn = MoveColumn Direction1D Bool deriving (Show, Read)
instance Message MoveColumn

data RotateColumns = RotateColumns Direction1D Bool deriving (Show, Read)
instance Message RotateColumns

data TidyColumns = TidyColumns deriving (Show, Read)
instance Message TidyColumns

instance LayoutClass ThreeColumnStable Window where
    doLayout :: ThreeColumnStable Window -> Rectangle -> W.Stack Window -> X ([(Window, Rectangle)], Maybe (ThreeColumnStable Window))
    doLayout (ThreeColumnStable delta ratio cols) r s =
      return (rects, Just (ThreeColumnStable delta ratio cols'))
        where
          sws = W.integrate s

          cols' =
            if length sws == 1 && hasDeleted cols sws
            then ([], [head sws], [])
            else
              insertNew permutedCols predWs
              where
                prunedCols = removeDeleted cols sws

                preservedWs = preserved prunedCols sws
                permutedCols = permute prunedCols preservedWs

                addedWs = added prunedCols sws
                predWs = zip (map (`predecessor` sws) addedWs) addedWs

          rects = zip sws (tileCols ratio r cols')


    handleMessage :: ThreeColumnStable Window -> SomeMessage -> X (Maybe (ThreeColumnStable Window))
    handleMessage l@(ThreeColumnStable delta frac cols) m
      | Just (MoveWindow dir wrap) <- fromMessage m = handleMove l (moveWindow dir wrap)
      | Just (MoveColumn dir wrap) <- fromMessage m = handleMove l (moveColumn dir wrap)
      | Just (RotateColumns dir wrap) <- fromMessage m = handleMove l (rotateColumns dir wrap)
      | Just TidyColumns <- fromMessage m = handleTidyColumns l
      | Just Shrink <- fromMessage m = return (Just (ThreeColumnStable delta (max 0 $ frac-delta) cols))
      | Just Expand <- fromMessage m = return (Just (ThreeColumnStable delta (min 1 $ frac+delta) cols))
      | otherwise = return Nothing

    description _ = "ThreeColumnStable"

handleMove :: ThreeColumnStable Window -> (Columns -> Window -> Columns) -> X (Maybe (ThreeColumnStable Window))
handleMove (ThreeColumnStable delta frac cols) f = do
  ws <- gets windowset
  let floating = W.floating ws
      mstack = W.stack . W.workspace . W.current $ ws
      tmstack = mfilter (\s -> M.notMember (W.focus s) floating) mstack
  traverse
    (\stack -> do
      let focus = W.focus stack
          cols' = f cols focus
          permutation = M.fromList (zip (uncols cols) (uncols cols'))
          ws' = map (\w -> M.findWithDefault w w permutation) (W.integrate stack)
          mstack' = fmap (focusWindow focus) (W.differentiate ws')
      _ <- replaceStack mstack'
      return (ThreeColumnStable delta frac cols')
    )
    tmstack
  where
    focusWindow focus s = until ((focus ==) . W.focus) W.focusUp' s
    uncols (l, c, r) = reverse l ++ reverse c ++ reverse r

moveWindow :: Direction1D -> Bool -> Columns -> Window -> Columns
moveWindow = withDirection moveWindow'
  where
    moveWindow' wrap cols@(l, c, r) w
      | w `elem` c = (w : l, c', r)
      | w `elem` r = (l, w : c, r')
      | w `elem` l && wrap = (l', c, w : r)
      | otherwise = cols
      where
        l' = filter (/= w) l
        c' = filter (/= w) c
        r' = filter (/= w) r

moveColumn :: Direction1D -> Bool -> Columns -> Window -> Columns
moveColumn = withDirection moveColumn'
  where
    moveColumn' wrap cols@(l, c, r) w
      | w `elem` c = (c, l, r)
      | w `elem` r = (l, r, c)
      | w `elem` l && wrap = (c, r, l)
      | otherwise = cols

rotateColumns :: Direction1D -> Bool -> Columns -> Window -> Columns
rotateColumns = withDirection rotateColumns'
  where
    rotateColumns' True (l, c, r) _ = (c, r, l)
    rotateColumns' False cols@(l, c, r) w
      | w `elem` l = cols
      | otherwise = (c, r, l)

withDirection :: (Bool -> Columns -> Window -> Columns) -> Direction1D -> Bool -> Columns -> Window -> Columns
withDirection f Prev wrap cols focus = f wrap cols focus
withDirection f Next wrap cols focus = reverseColumns (f wrap (reverseColumns cols) focus)
  where
    reverseColumns (l, c, r) = (r, c, l)

handleTidyColumns :: ThreeColumnStable Window -> X (Maybe (ThreeColumnStable Window))
handleTidyColumns l@(ThreeColumnStable delta frac cols) = do
  return (Just (ThreeColumnStable delta frac (adjustColumns cols)))
  where
    adjustColumns ([], [], rs) = ([], rs, [])
    adjustColumns (ls, [], rs) = ([], ls, rs)
    adjustColumns cs = cs

replaceStack :: Maybe (W.Stack Window) -> X ()
replaceStack s = do
  st <- get
  let wset = windowset st
      cur  = W.current wset
      wsp  = W.workspace cur
  put st{windowset=wset{W.current=cur{W.workspace=wsp{W.stack=s}}}}

hasDeleted :: Columns -> [Window] -> Bool
hasDeleted (l, c, r) windows =
  all (`elem` lcr) windows && length windows < length lcr
    where
      lcr = l ++ c ++ r

removeDeleted :: Columns -> [Window] -> Columns
removeDeleted (l, c, r) windows = (l', c', r')
  where
    l' = filter (`elem` windows) l
    c' = filter (`elem` windows) c
    r' = filter (`elem` windows) r

added :: Columns -> [Window] -> [Window]
added (l, c, r) windows = filter (`notElem` (l ++ c ++ r)) windows

preserved :: Columns -> [Window] -> [Window]
preserved (l, c, r) windows = filter (`elem` (l ++ c ++ r)) windows

permute :: Columns -> [Window] -> Columns
permute (l, c, r) windows = (l', c', r')
  where
    l' = reverse (take (length l) windows)
    c' = reverse (take (length c) (drop (length l) windows))
    r' = reverse (take (length r) (drop (length l + length c) windows))

predecessor :: Window -> [Window] -> Maybe Window
predecessor _ [] = Nothing
predecessor _ [_] = Nothing
predecessor target (x:y:xs)
  | y == target = Just x
  | otherwise = predecessor target (y:xs)

insertNew :: Columns -> [(Maybe Window, Window)] -> Columns
insertNew cs ws = foldl insertOne cs ws

insertOne :: Columns -> (Maybe Window, Window) -> Columns
insertOne ([ ], [ ], [ ]) (_,         w) = ([ ], [w], [ ])
insertOne (l:ls, cs, rs) (Nothing,   w) = (w:l:ls, cs, rs)
insertOne ([], c:cs, rs) (Nothing,   w) = ([w], c:cs, rs)
insertOne ([], [], rs) (Nothing,   w) = ([], [w], rs)
insertOne (ls, c:cs, [ ]) ((Just p),  w)
  | c == p = (ls, c:cs, [w])
insertOne ([ ], cs, r:rs) ((Just p),  w)
  | r == p = (cs, r:rs, [w])
insertOne (l:ls, [], rs) ((Just p),  w)
  | l == p  = (l:ls, [w], rs)
insertOne (ls, cs, rs) ((Just p),  w)
  | p `elem` ls = (addAfter p ls w, cs, rs)
  | p `elem` cs = (ls, addAfter p cs w, rs)
  | p `elem` rs = (ls, cs, addAfter p rs w)
insertOne _ _ = error "insertOne: predecessor not found"

addAfter :: Window -> Column -> Window -> Column
addAfter p (hd : tl) w
  | p == hd = w : hd : tl
  | otherwise = hd : addAfter p tl w

tileCols :: Rational -> Rectangle -> Columns -> [Rectangle]
tileCols f r (ls, cs, rs) = lRs ++ cRs ++ rRs
  where (lR, cR, rR) = split3HorizontallyBy f r
        lRs = splitVertically' (length ls) lR
        cRs = splitVertically' (length cs) cR
        rRs = splitVertically' (length rs) rR

splitVertically' :: Int -> Rectangle -> [Rectangle]
splitVertically' 0 _ = []
splitVertically' n r = splitVertically n r

-- | Divide the screen into three rectangles, using a rational to specify the ratio of center one
split3HorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
  ( Rectangle sx sy sidew sh
  , Rectangle (sx + fromIntegral sidew) sy middlew sh
  , Rectangle (sx + fromIntegral sidew + fromIntegral middlew) sy sidew sh
  )
  where middlew = floor $ fromIntegral sw * f
        sidew = (sw - fromIntegral middlew) `div` 2
