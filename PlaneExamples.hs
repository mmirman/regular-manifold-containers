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
import Control.Comonad

integers = makePlane fL fR fD fU (fL . fD) (fR . fD) (fL . fU) (fR . fU)
  where fL (i,j) = (i-1,j)
        fR (i,j) = (i+1,j)
        fD (i,j) = (i,j-1)
        fU (i,j) = (i,j+1)
