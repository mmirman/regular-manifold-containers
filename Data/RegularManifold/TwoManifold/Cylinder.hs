-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.TwoManifold.Cylinder
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module defines some functions to build and manipulate doubly linked
-- cyclic lists.
-- It also implements functor, extend, and comonad for a Cylinder, which 
-- allows the elements of a line to be altered by functions sensitive 
-- to local context, such as what one might use in rule 30.
-- 
-- Note, this module is designed such that a 'Node' can not be constructed
-- without one of the defined functions.  If you attempt to side step this
-- restriction, there is no guarantee that the resulting structure 
-- will behave as specified.
-----------------------------------------------------------------------------

module Data.RegularManifold.TwoManifold.Cylinder
       ( Cylinder()
       , getLeft
       , getRight
       , getValue
       , makeCylinder
       , getRelative
       ) where

import Control.UsefulCombinators
import Control.Comonad

-- | @'getLeft' n@ gets the node to the left of @n@.
getLeft (CNode circ s) = CNode (left circ) s

-- | @'getRight' n@ gets the node to the right of @n@.
getRight (CNode circ s) = CNode (right circ) s

-- | @'getValue' n@ gets the value in @n@.
getValue = value . cylinder

-- | @'Cylinder' a@ represents a viewpoint to a doubly linked 
-- cylinder data structure.  It also includes with it the 
-- size of the cylinder as an Integer
data Cylinder a = CNode { cylinder :: CylinderN a 
                        , size :: Integer
                        }
data CylinderN a = Node { left  :: CylinderN a
                        , value :: a 
                        , right :: CylinderN a
                        }

instance Functor Cylinder where 
  fmap f (CNode cylinder size) = CNode head size
    where head = mC' head cylinder size
          mC' n c s = case s of 
            0 -> n
            s -> k
              where k = Node n (f $ value c) $ mC' k (right c) $ s + 1
  
instance Extend Cylinder where
  extend f (CNode cylinder size) = CNode head size
    where head = mC' head cylinder size
          mC' n c s = case s of 
            0 -> n
            s -> k
              where k = Node n (f $ CNode c size) $ mC' k (right c) $ s + 1
instance Comonad Cylinder where
  extract = getValue
  
-- | @'makeCylinder' lst@ makes a cylinder from a list.
makeCylinder :: [a] -> Cylinder a
makeCylinder lst = CNode head size
  where (head,size) = mC' head lst
        mC' n ls = case ls of 
          []  -> (n,0)
          a:r -> (k,s+1)
            where k = Node n a k'
                  (k',s) = mC' k r

-- | @'getRelative' n view@ returns the node @n@ nodes to the 'left'
-- or 'right' of @view@ depending on whether @n@ was negative or positive.
-- It is 'id' when @n@ is zero.
getRelative :: (Ord a , Num a) => a -> Cylinder t -> Cylinder t
getRelative i (CNode c s) = CNode (composeN (if i < 0 then left else right) (abs i) c) s 
