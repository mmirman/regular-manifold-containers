-----------------------------------------------------------------------------
-- |
-- Module      :  LineExamples
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
--  Some examples of initialization of the data structures.
-----------------------------------------------------------------------------

module LineExamples where

import Data.RegularManifold.OneManifold.Line
import Control.Comonad

integers = makeLine (-1 +) (+ 1) 0

integersAtTen = rewriteNode 10 integers

integersBelowZeroFive = mapLineAlternate (const 5) id integers

evenIntegers = fmap (* 2) integers

rule30update cell = 
  case (getValue $ getLeft cell, getValue cell, getValue $ getRight cell) of
    (True, True, _) -> False
    (True, False, True) -> False
    (False, False, False) -> False
    _ -> True

rule30 :: [Line Bool]
rule30 = iterate (extend rule30update) $ makeLine (const False) (const False) True 
    
