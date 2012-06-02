{-# LANGUAGE 
 FlexibleInstances
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CellularAutomata.Life
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Abstract game of life.
-----------------------------------------------------------------------------

module CellularAutomata.Life ( nextGen
                             , conway'sGameOfLife
                             ) where

import Data.RegularManifold.ContextualClass

import Control.Comonad
import Debug.Trace

within s v l = s <= v && v <= l

nextGen :: (Comonad w, Contextual (w Bool)) => Int -> Int -> w Bool -> w Bool
nextGen s l = extend $ within s l . length . filter extract . getContext

conway'sGameOfLife :: (Comonad w, Contextual (w Bool)) => w Bool -> [w Bool]
conway'sGameOfLife = iterate $ nextGen 2 3

