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
-- This module defines some functions to build and manipulate infinite lines.
-- It also implements functor, extend, and comonad for a Line, which allows
-- the elements of a line to be altered by functions sensitive 
-- to local context, such as what one might use in rule 30.
-- 
-- Note, this module is designed such that a 'Node' can not be constructed
-- without one of the defined functions.  If you attempt to side step this
-- restriction, there is no guarantee that the resulting structure 
-- will behave as specified.
-----------------------------------------------------------------------------

module Data.RegularManifold.OneManifold.Line 
       ( Line()
       , getLeft
       , getRight
       , getValue
       , makeLine
       , rewriteNode
       , getRelative
       , rewriteRelative
       , mapLineAlternate
       ) where

import Control.UsefulCombinators
import Control.Comonad

-- | @'getLeft' n@ gets the node to the left of @n@.
getLeft = left
-- | @'getRight' n@ gets the node to the right of @n@.
getRight = right
-- | @'getValue' n@ gets the value in @n@.
getValue = value

-- | @'Line' a@ represents a viewpoint to a doubly linked 
-- Line data structure of infinite length. 
data Line a = Node { left  :: Line a
                   , value :: a 
                   , right :: Line a
                   }

instance Functor Line where 
  fmap f (Node l v r) = iC
    where iC = Node iL (f v) iR
          iL = mapLineL (f . value) l iC
          iR = mapLineR (f . value) r iC

instance Extend Line where
  extend f v@(Node l _ r) = iC
    where iC = Node iL (f v) iR
          iL = mapLineL f l iC
          iR = mapLineR f r iC

instance Comonad Line where
  extract = value

mapLineL f v@(Node l _ _) r = k
  where k = Node (mapLineL f l k) (f v) r

mapLineR f v@(Node _ _ r) l = k
  where k = Node l (f v) (mapLineR f r k)

-- | @'makeLine' fL fR init@ acts like 'iterate' in in two directions
-- where @fL@ is iterated to the left, and @fR@ is iterated to the right
-- both starting with the value @init@.  The result is a doubly linked
-- line.
makeLine :: (t -> t) -> (t -> t) -> t  -> Line t
makeLine fL fR init = i
  where i = Node iL init iR
        iL = mapLineL (fL . value) i i
        iR = mapLineR (fR . value) i i

-- | @'rewriteNode' a view@ returns a new infinite list with @a@
-- as the new viewpoint and all other nodes the same as in @view@
rewriteNode :: t -> Line t -> Line t
rewriteNode a (Node l _ r) = fmap value $ makeLine left right $ Node l a r

-- | @'getRelative' n view@ returns the node @n@ nodes to the 'left'
-- or 'right' of @view@ depending on whether @n@ was negative or positive.
-- It is 'id' when @n@ is zero.
getRelative :: (Ord a , Num a) => a -> Line t -> Line t
getRelative i = composeN (if i < 0 then left else right) (abs i)

-- | @'rewriteRelative' n v view@ returns the infinite list with @v@ 
-- in the @n@ position relative to @view@.
rewriteRelative :: (Ord a , Num a) => a -> t -> Line t -> Line t
rewriteRelative i t = getRelative (-i) . rewriteNode t . getRelative i

-- | @'mapLineAlternate' fL fR line@ maps @fL@ down the left side of @line@
-- and @fR@ down the right side of @line@. It leaves the view node alone.
mapLineAlternate :: (t -> t) -> (t -> t) -> Line t -> Line t
mapLineAlternate fl fr = fmap value . makeLine (getBy left fl) (getBy right fr)
  where getBy by f n = n' { value = f $ value $ n' }
          where n' = by n