-----------------------------------------------------------------------------
-- |
-- Module      :  PlaneExamples
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
--  Some examples of initialization of the data structures.
-----------------------------------------------------------------------------

module PlaneExamples where

import Data.RegularManifold.TwoManifold.Plane
import Data.RegularManifold.TwoManifold.TwoManifoldClass
import Control.Comonad

integers = coordinates :: Plane (Integer, Integer)


