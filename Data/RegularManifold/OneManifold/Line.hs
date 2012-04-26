{-# LANGUAGE Trustworthy #-}
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
       , modifyNode
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

mapLineL f (Node l _ p) r = k
  where k = Node (mapLineL f l k) r (f p)

mapLineR f (Node _ r p) l = k
  where k = Node l (mapLineR f r k) (f p)

-- | @'makeLine' fL fR init@ acts like 'iterate' in two dimensions
-- where @fL@ is iterated to the left, and @fR@ is iterated to the right
-- both starting with the value @init@.  The result is a doubly linked
-- line.
makeLine :: (t -> t) -> (t -> t) -> t -> D1View t
makeLine fL fR init = i
  where i = Node iL iR init
        iL = mapLineL fL i i
        iR = mapLineR fR i i

modifyNode :: D1View t -> t -> D1View t
modifyNode (Node l r _) a = fmap value $ makeLine left right (Node l r a)

getRelative :: Integer -> D1View t -> D1View t
getRelative i = composeN (abs i) (if i < 0 then left else right) (abs i)
