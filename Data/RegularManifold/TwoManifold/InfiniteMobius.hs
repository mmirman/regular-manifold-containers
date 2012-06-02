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

module Data.RegularManifold.TwoManifold.InfiniteMobius ( InfiniteMobius()
                                                       , makeInfMobius
                                                       ) where
import Control.UsefulCombinators
import Control.Comonad
import Data.RegularManifold.TwoManifold.TwoManifoldClass
import Data.Tuple (swap)

instance TwoManifold (InfiniteMobius a) where
  getLeft (CNode m n) = CNode (left m) n
  getRight (CNode m n) = CNode (right m) n
  getUp (CNode m n) = CNode (up m) n
  getDown (CNode m n) = CNode (down m) n

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

makeInfMobius :: Integer -> InfiniteMobius (Integer, Integer)
makeInfMobius n = CNode outHead n  
  where (outHead, inHead) = makeStrip 0 (outHead, inHead)
        
        makeStrip v (pOutHead,pInHead) = (fOutHead, fInHead)
          where ~(sOutHead,sInHead) = makeStrip (v+1) (fOutHead, fInHead)
                ~(fOutHead,fOutTail) = makeWith v 0 fInTail sOutHead pInHead fInHead
                ~(fInHead,fInTail)   = makeWith v n fOutTail pOutHead sInHead fOutHead
        
        makeWith v io = makeWith' (n - 1) where
          makeWith' c prev' up' down' end' = case c of
            0 -> (tail', tail')
              where tail' = Node prev' up' (v,io) down' end'
            _ -> (curr',tail')
              where curr' = Node prev' up' (v,c + io) down' next'
                    ~(next',tail') = makeWith' (c-1) curr' (right up') (right down') end'
        
instance Functor InfiniteMobius where
  fmap = extend . (. extract)
  
instance Extend InfiniteMobius where
  extend f (CNode outHeadOrg n) = CNode outHead n
    where inHeadOrg = composeN left n outHeadOrg

          (outHead, inHead) = makeStrip (outHeadOrg, inHeadOrg) (outHead, inHead)
        
          makeStrip (outOrg,inOrg) (pOutHead,pInHead) = (fOutHead, fInHead)
            where ~(sOutHead,sInHead) = makeStrip (up outOrg, down inOrg) (fOutHead, fInHead)
                  ~(fOutHead,fOutTail) = makeWith outOrg fInTail sOutHead pInHead fInHead
                  ~(fInHead,fInTail)   = makeWith inOrg fOutTail pOutHead sInHead fOutHead
        
          makeWith v = makeWith' (n - 1) where
            makeWith' c prev' up' down' end' = case c of
              0 -> (tail', tail')
                where tail' = Node prev' up' (f $ CNode v n) down' end'
              _ -> (curr',tail')
                where curr' = Node prev' up' (f $ CNode v n) down' next'
                      ~(next',tail') = makeWith' (c-1) curr' (right up') (right down') end'

instance Comonad InfiniteMobius where
  extract = value . mobius