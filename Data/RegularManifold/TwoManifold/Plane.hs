-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.TwoManifold.Plane
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

module Data.RegularManifold.TwoManifold.Plane
       ( Plane()
       , getLeft
       , getRight
       , getUp
       , getDown
       , getValue
       , makePlane
       ) where

import Control.UsefulCombinators
import Control.Comonad

-- | @'getLeft' n@ gets the node to the left of @n@.
getLeft = left
-- | @'getRight' n@ gets the node to the right of @n@.
getRight = right

-- | @'getUp' n@ gets the node above @n@.
getUp = up
-- | @'getDown' n@ gets the node below @n@.
getDown = down
-- | @'getValue' n@ gets the value in @n@.
getValue = value

-- | @'Line' a@ represents a viewpoint to a doubly linked 
-- Line data structure of infinite length. 
data Plane a = Node { left  :: Plane a
                    , right :: Plane a
                    , down  :: Plane a
                    , up    :: Plane a
                    , value :: a                       
                    }

instance Functor Plane where 
  fmap f = undefined

instance Extend Plane where
  extend f = undefined

instance Comonad Plane where
  extract = value

type I2 = (Integer,Integer)        

mapPlaneL f v@(Node l _ _ _ _) r d u = k
  where k = Node (mapPlaneL f l k (left d) (left u)) r d u (f v)

mapPlaneR f l v@(Node _ r _ _ _) d u = k
  where k = Node l (mapPlaneR f k r (right d) (right u)) d u (f v)
        
mapPlaneD f l r v@(Node _ _ d _ _) u = k
  where k = Node l r (mapPlaneD f (down l) (down r) d k) u (f v)
        
mapPlaneU f l r d v@(Node _ _ _ u _) = k
  where k = Node l r d (mapPlaneU f (up l) (up r) k u) (f v)

mapPlaneLU f fL fU v d r = p
  where p  = Node l r d u (f v)
        l = mapPlaneL fL p p (left d) lu
        u = mapPlaneU fU lu (up r) p p
        lu = mapPlaneLU f fL fU p l u

mapPlaneLD f fL fD v u r = p
  where p  = Node l r d u (f v)
        l = mapPlaneL fL p p ld (left u) 
        d = mapPlaneD fD ld (down r) p p
        ld = mapPlaneLD f fL fD p l d
        
mapPlaneRU f fR fU v d l = p
  where p  = Node l r d u (f v)
        r = mapPlaneR fR p p (right d) ru
        u = mapPlaneU fU (up l) ru p p
        ru = mapPlaneRU f fR fU p r u
        
mapPlaneRD f fR fD v u l = p
  where p  = Node l r d u (f v)
        r = mapPlaneR fR p p rd (right u) 
        d = mapPlaneD fD (down l) rd p p
        rd = mapPlaneRD f fR fD p r d

-- | @'makePlane' fL fR fU fD init@ acts like a two dimensional
-- iterate
makePlane fL fR fD fU fLD fRD fLU fRU = p
  where p = Node l r d u (0,0)
        l = mapPlaneL (fL . value) p p ld lu
        r = mapPlaneR (fR . value) p p rd ru
        d = mapPlaneD (fD . value) ld rd p p
        u = mapPlaneU (fU . value) lu ru p p
        
        lu = mapPlaneLU (fLU . value) (fL . value) (fU . value) p l u
        ld = mapPlaneLD (fLD . value) (fL . value) (fD . value) p l d
        ru = mapPlaneRU (fRU . value) (fR . value) (fU . value) p r u
        rd = mapPlaneRD (fRD . value) (fR . value) (fD . value) p r d
