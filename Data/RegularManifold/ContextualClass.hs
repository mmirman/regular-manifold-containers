-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.ContextualClass
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- 
-----------------------------------------------------------------------------

module Data.RegularManifold.ContextualClass where

class Contextual m where
  getContext :: m -> [m]