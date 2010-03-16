{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.Basics
    Description :  interval datatype and its basic instances 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval datatype and its basic instances.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.Basics 
(
   Interval(..)
)
where

import Prelude hiding (LT)


import Numeric.AERN.Basics.PartialOrdering

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck

import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.CInterval.Granularity
import Numeric.AERN.Basics.CInterval.Equality

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

{-|
    Pairs of endpoints.  An end user should not use this type directly
    but use the classes of which this is an instance.
-}
data Interval e =
    Interval
    { 
        lowEndpoint :: e,
        highEndpoint :: e
    }
    deriving (Eq)
    
instance (Show e) => (Show (Interval e))
    where
    show (Interval l h) = "[" ++ show l ++ "," ++ show h ++ "]"

instance CInterval (Interval e) where
    type Endpoint (Interval e) = e
    getEndpoints (Interval l h) = (l, h)
    fromEndpoints (l,h) = Interval l h     
    mapEndpoints f (Interval l h) = Interval (f l) (f h)
    mapBothEndpoints fl fh (Interval l h) = Interval (fl l) (fh h)
    mapEndpointPair f (Interval l h) = 
        Interval l2 h2
        where
        (l2, h2) = f (l, h)

-- random generation of intervals with no guarantee of consistency: 
instance (ArbitraryOrderedTuple e) => Arbitrary (Interval e)
    where
    arbitrary = 
        do
        (UniformlyOrderedPair (l,h)) <- arbitrary 
        return $ fromEndpoints (l,h)

{-| type for random generation of consistent intervals -}       
data ConsistentInterval e = ConsistentInterval (Interval e) deriving (Show)
        
instance (ArbitraryOrderedTuple e) => (Arbitrary (ConsistentInterval e))
    where
    arbitrary =
      case arbitraryPairRelatedBy LT of
          Just gen ->
              do
              (l,h) <- gen
              shouldBeSingleton <- arbitraryBoolRatio 1 10
              case shouldBeSingleton of
                  True -> return $ ConsistentInterval (Interval l l) 
                  False -> return $ ConsistentInterval (Interval l h)


{-| type for random generation of anti-consistent intervals -}        
data AntiConsistentInterval e = AntiConsistentInterval (Interval e) deriving (Show)
        
instance (ArbitraryOrderedTuple e) => (Arbitrary (AntiConsistentInterval e))
    where
    arbitrary =
      case arbitraryPairRelatedBy LT of
          Just gen ->
              do
              (l,h) <- gen 
              shouldBeSingleton <- arbitraryBoolRatio 1 10
              case shouldBeSingleton of
                  True -> return $ AntiConsistentInterval (Interval l l) 
                  False -> return $ AntiConsistentInterval (Interval h l)

{-| type for random generation of consistent and anti-consistent intervals 
    with the same probability -}        
data ConsistentOrACInterval e = ConsistentOrACInterval (Interval e) deriving (Show)
        
instance (ArbitraryOrderedTuple e) => (Arbitrary (ConsistentOrACInterval e))
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

instance (HasGranularity e, NumOrd.Lattice (Granularity e)) => 
         HasGranularity (Interval e)
    where
    type Granularity (Interval e) = Granularity e
    getGranularity = getGranularityInterval

instance (CanSetGranularityRoundedByNumericOrder e, NumOrd.Lattice (Granularity e)) => 
         CanSetGranularityRoundedByRefinementOrder (Interval e)
    where
    setGranularityOut = setGranularityOutInterval
    setGranularityIn = setGranularityInInterval
    setMinGranularityOut = setMinGranularityOutInterval
    setMinGranularityIn = setMinGranularityInInterval

    