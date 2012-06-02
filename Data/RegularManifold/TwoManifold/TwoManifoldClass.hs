{-# LANGUAGE 
 FlexibleInstances
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.TwoManifold.TwoManifoldClass
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.RegularManifold.TwoManifold.TwoManifoldClass ( TwoManifold(..)
                                                         ) where

import Data.RegularManifold.ContextualClass
import Control.Comonad

class TwoManifold m where
  getLeft, getRight, getUp, getDown :: m a -> m a

instance (Comonad m, TwoManifold m) => Contextual (m a) where
  getContext v = map (($ v))
               $ tail [ h . v | h <- [id, getLeft, getRight] , v <- [id, getUp, getDown] ]