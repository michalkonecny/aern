{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.Basics
    Description :  consistency instances for intervals 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Consistency instances for intervals.
    
    This is a hidden module reexported via its parent.
-}

module Numeric.AERN.Basics.Interval.Consistency 
(
   ConsistentInterval(..),
   AntiConsistentInterval(..),
   ConsistentOrACInterval(..)
)
where

import Prelude hiding (LT,EQ)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Basics.Interval.Basics

import Numeric.AERN.Basics.Consistency

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort


import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)


instance (NumOrd.PartialComparison e) => HasConsistency (Interval e)
    where
    type ConsistencyEffortIndicator (Interval e) = 
        NumOrd.PartialCompareEffortIndicator e
    consistencyDefaultEffort (Interval l r) =
        NumOrd.pCompareDefaultEffort l
    isConsistentEff effort (Interval l r) =
        NumOrd.pLeqEff effort l r

instance (NumOrd.PartialComparison e) => HasAntiConsistency (Interval e)
    where
    isAntiConsistentEff effort (Interval l r) = 
        NumOrd.pLeqEff effort r l
    flipConsistency (Interval l r) = Interval r l

instance HasThinRepresentative (Interval e)
    where
    getThinRepresentative (Interval l r) = Interval r r

-- random generation of intervals with no guarantee of consistency: 
instance (NumOrd.ArbitraryOrderedTuple e) => Arbitrary (Interval e)
    where
    arbitrary = 
        do
        (NumOrd.UniformlyOrderedPair (l,r)) <- arbitrary 
        return $ Interval l r

{-| type for random generation of consistent intervals -}       
data ConsistentInterval e = ConsistentInterval (Interval e) deriving (Show)
        
instance (NumOrd.ArbitraryOrderedTuple e) => (Arbitrary (ConsistentInterval e))
    where
    arbitrary =
      case NumOrd.arbitraryPairRelatedBy LT of
          Just gen ->
              do
              (l,r) <- gen
              shouldBeSingleton <- arbitraryBoolRatio 1 10
              case shouldBeSingleton of
                  True -> return $ ConsistentInterval (Interval l l) 
                  False -> return $ ConsistentInterval (Interval l r)


{-| type for random generation of anti-consistent intervals -}        
data AntiConsistentInterval e = AntiConsistentInterval (Interval e) deriving (Show)
        
instance (NumOrd.ArbitraryOrderedTuple e) => (Arbitrary (AntiConsistentInterval e))
    where
    arbitrary =
      case NumOrd.arbitraryPairRelatedBy LT of
          Just gen ->
              do
              (l,r) <- gen 
              shouldBeSingleton <- arbitraryBoolRatio 1 10
              case shouldBeSingleton of
                  True -> return $ AntiConsistentInterval (Interval l l) 
                  False -> return $ AntiConsistentInterval (Interval r l)

{-| type for random generation of consistent and anti-consistent intervals 
    with the same probability -}        
data ConsistentOrACInterval e = ConsistentOrACInterval (Interval e) deriving (Show)
        
instance (NumOrd.ArbitraryOrderedTuple e) => (Arbitrary (ConsistentOrACInterval e))
    where
    arbitrary =
      do
      consistent <- arbitrary
      case consistent of
          True ->
              do
              (ConsistentInterval i) <- arbitrary
              return $ ConsistentOrACInterval i
          False ->
              do
              (AntiConsistentInterval i) <- arbitrary
              return $ ConsistentOrACInterval i

