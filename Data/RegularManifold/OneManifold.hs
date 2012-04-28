-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.OneManifold
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module defines a 1-Manifold datastructure, which can corrospond to 
-- a doubly linked list with no start or end
-----------------------------------------------------------------------------
module Data.RegularManifold.OneManifold
       ( D1View(..)
       ) where

-- | @'D1View' a@ represents a viewpoint to a doubly linked 
-- data structure
-- in one dimension.  It can only be cyclic or infinite.
data D1View a = Node { left  :: D1View a
                     , right :: D1View a
                     , value :: a 
                     }
