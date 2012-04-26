{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Examples
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
--  Some examples of initialization of the data structures.
-----------------------------------------------------------------------------

module Examples where

import Data.RegularManifold.OneManifold.Line

integers = makeLine (-1 +) (+ 1) 0

integersAtTen = modifyNode integers 10

evenIntegers = fmap (* 2) integers