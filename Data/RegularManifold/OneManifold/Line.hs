-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.OneManifold.Line
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module defines some functions to build and manipulate infinite lines
-----------------------------------------------------------------------------

module Data.RegularManifold.OneManifold.Line 
       ( D1View(..)
       , makeLine
       , rewriteNode
       , getRelative
       , rewriteRelative
       , mapLineAlternate
       ) where

import Data.RegularManifold.OneManifold
import Control.UsefulCombinators

-- | Invariant:  The D1View passed to this instance of fmap 
-- is isomorphic to a line.
instance Functor D1View where 
  fmap f (Node l r v) = iC
    where iC = Node iL iR (f v)
          iL = mapLineL f l iC
          iR = mapLineR f r iC

mapLineL f (Node l _ v) r = k
  where k = Node (mapLineL f l k) r (f v)

mapLineR f (Node _ r v) l = k
  where k = Node l (mapLineR f r k) (f v)

-- | @'makeLine' fL fR init@ acts like 'iterate' in two dimensions
-- where @fL@ is iterated to the left, and @fR@ is iterated to the right
-- both starting with the value @init@.  The result is a doubly linked
-- line.
makeLine :: (t -> t) -> (t -> t) -> t -> D1View t
makeLine fL fR init = i
  where i = Node iL iR init
        iL = mapLineL fL i i
        iR = mapLineR fR i i

-- | @'rewriteNode' a view@ returns a new infinite list with @a@
-- as the new viewpoint and all other nodes the same as in @view@
rewriteNode :: t -> D1View t -> D1View t
rewriteNode a (Node l r _) = fmap value $ makeLine left right (Node l r a)

-- | @'getRelative' n view@ returns the node @n@ nodes to the 'left'
-- or 'right' of @view@ depending on whether @n@ was negative or positive.
-- It is 'id' when @n@ is zero.
getRelative :: (Ord a , Num a) => a -> D1View t -> D1View t
getRelative i = composeN (if i < 0 then left else right) (abs i)

-- | @'rewriteRelative' n v view@ returns the infinite list with @v@ 
-- in the @n@ position relative to @view@.
rewriteRelative :: (Ord a , Num a) => a -> t -> D1View t -> D1View t
rewriteRelative i t = getRelative (-i) . rewriteNode t . getRelative i

-- | @'mapLineAlternate' fL fR line@ maps @fL@ down the left side of @line@
-- and @fR@ down the right side of @line@. It leaves the view node alone.
mapLineAlternate :: (t -> t) -> (t -> t) -> D1View t -> D1View t
mapLineAlternate fl fr = fmap value . makeLine left' right'
  where left' l = (left l) { value = fl $ value $ left l }
        right' r = (left r) { value = fr $ value $ right r }