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
-- This module defines some constructors for infinite lines, and a Functor
-- instance for infinite lines.  
-----------------------------------------------------------------------------

module Data.RegularManifold.OneManifold.Line 
       (D1View(..)
       , makeLine
       ) where

import Data.RegularManifold.OneManifold

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
makeLine fL fR init = i
  where i = Node iL iR init
        iL = mapLineL fL i i
        iR = mapLineR fR i i

