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

module Main where
import CellularAutomata.Life
import Control.Comonad
import Data.RegularManifold.TwoManifold.InfiniteMobius


main = do 
  putStrLn "Please enter a diameter: "
  size <- readLn 
  let mobiusLife = conway'sGameOfLife $ extend lf (makeInfMobius size)
      
      lf k = elem (extract k) [ (0,0), (0,1) , (0,2), (1,2) , (2 ,1 ) ]
      
  
  putStrLn "here we go man"