-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RegularManifold.TwoManifold.InfiniteMobius
-- Copyright   :  (c) Matthew Mirman 2012
-- License     :  GPL-3 
-- 
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module defines some functions to build and manipulate doubly linked
-- cyclic lists.
-- It also implements functor, extend, and comonad for a InfiniteMobius, which 
-- allows the elements of a line to be altered by functions sensitive 
-- to local context, such as what one might use in rule 30.
-- 
-- Note, this module is designed such that a 'Node' can not be constructed
-- without one of the defined functions.  If you attempt to side step this
-- restriction, there is no guarantee that the resulting structure 
-- will behave as specified.
-----------------------------------------------------------------------------

module Data.RegularManifold.TwoManifold.InfiniteMobius where

import Control.UsefulCombinators
import Control.Comonad
import Data.Tuple (swap)

-- | @'InfiniteMobius' a@ represents a viewpoint to a doubly linked 
-- mobius data structure.  It also includes with it the 
-- size of the mobius as an Integer
data InfiniteMobius a = CNode { mobius :: InfiniteMobiusN a 
                              , size :: Integer
                              }
data InfiniteMobiusN a = Node { left  :: InfiniteMobiusN a
                              , up    :: InfiniteMobiusN a
                              , value :: a 
                              , down  :: InfiniteMobiusN a                                
                              , right :: InfiniteMobiusN a
                              }

iff t b = if b then t else id
   
infMobiusN :: Integer -> InfiniteMobius (Integer, Integer)
infMobiusN n = CNode (head f) n
  where total = 2 * n
        
        getPrev i = fromInteger $ (i - 1) `mod` total
        getNext i = fromInteger $ (i + 1) `mod` total
          
        makeFromI v f s i = Node (f !! getNext i) u (v, i) d (f !! getPrev i)
          where (u,d) = swap `iff` (i >= n) $ (f !! fromInteger ((i + n) `mod` total), s !! fromInteger i)
          
        makeLfromI v f s = flip map [0..total] $ makeFromI v f s
        
        f = makeLfromI 0 f s
        s = edge 1 f
        
        edge v p = c
          where n = edge (v+1) c
                c = makeLfromI v c n
{-
prod :: !A -> B
prod blah = (single :: A -> B) c 
  where c = blah

prod :: A -> B
prod blah = (single :: A -> B) blah 

prod :: A -> B
prod blah = (single :: A -> B) blah 

           . ; a : A |-  a : A
       ----------------------------
       . ; . |- (\a . a) : (A -> A)
     ---------------------------------
      . ; . |- !(\a . a) : !(A -> A)

inferring bang - check if a use occurs more than once, and place a bang around it if it does (and 
only if it does).  start with the smallest expressions first.  




\blah r . (\c . c ) ((\f.(\x.xx) (\x.f (xx) )) blah) 




prod :: !Blah -> t
prod blah = c

   t2 -> t3
--------------------
  \x:t.a : t2

-}