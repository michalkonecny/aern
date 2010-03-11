{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval
    Description :  a minimal interval datatype  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    A minimal interval datatype.
-}
module Numeric.AERN.Basics.Interval 
(
   Interval(..)
)
where

import Prelude hiding (LT)

import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.CInterval

import Numeric.AERN.Basics.PartialOrdering

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck

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
instance (Arbitrary e) => Arbitrary (Interval e)
    where
    arbitrary = arbitrary >>= return . fromEndpoints

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
