{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Comparison
    Description :  partially ordered sets using numeric order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Partially ordered sets using numeric order notation.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.Comparison where

import qualified Prelude
import Prelude hiding (Eq, (==), compare, EQ, LT, GT, (<), (<=), (>=), (>))

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.NumericOrder.PartialComparison
import Numeric.AERN.Basics.Laws.Relation

import Numeric.AERN.Misc.Bool

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A partially ordered set.
    
    (More-or-less copied from Data.Comparison 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class (PartialComparison t, Show t) => Comparison t where
    compare :: t -> t -> PartialOrdering
    
    -- default implementation assuming the inherited partial comparison is actually total:
    compare a b =
        case pCompare a b of
            Just r -> r
            _ -> error $
                "Comparison comparison of " ++ show a
                ++ " with " ++ show b ++ " is not decidable"
    
    -- | non-reflexive inequality
    (==)  :: t -> t -> Bool
    -- | Is comparable to.
    (<==>)  :: t -> t -> Bool
    -- | Is not comparable to.
    (</=>)  :: t -> t -> Bool
    (<)     :: t -> t -> Bool
    (<=)    :: t -> t -> Bool
    (>=)    :: t -> t -> Bool
    (>)     :: t -> t -> Bool

    -- defaults using compare:
    a ==   b = a `compare` b Prelude.== EQ
    a <    b = a `compare` b Prelude.== LT
    a >    b = a `compare` b Prelude.== GT
    a <==> b = a `compare` b Prelude./= NC
    a </=> b = a `compare` b Prelude.== NC
    a <=   b = (a `compare` b) `elem` [EQ, LT, LEE]
    a >=   b = (a `compare` b) `elem` [EQ, GT, GEE]

instance Comparison Int where
    compare a b = toPartialOrdering $ Prelude.compare a b  

propComparisonIllegalArgException :: 
    (Comparison t) => t -> t -> Bool
propComparisonIllegalArgException illegalArg e =
    and $ map raisesAERNException 
                [compare e illegalArg, compare illegalArg e] 

propComparisonReflexiveEQ :: (Comparison t) => t -> t -> Bool
propComparisonReflexiveEQ _ e = 
    compare e e Prelude.== EQ 

propComparisonAntiSymmetric :: (Comparison t) => t -> UniformlyOrderedPair t -> Bool
propComparisonAntiSymmetric _ (UniformlyOrderedPair (e1, e2)) = 
    compare e2 e1 Prelude.== (partialOrderingTranspose $ compare e1 e2) 

propComparisonTransitiveEQ :: (Comparison t) => t -> t -> t -> t -> Bool
propComparisonTransitiveEQ _ e1 e2 e3 = transitive (==) e1 e2 e3
    
propComparisonTransitiveLT :: (Comparison t) => t -> t -> t -> t -> Bool
propComparisonTransitiveLT _ e1 e2 e3 = transitive (<) e1 e2 e3
    
propComparisonTransitiveLE :: (Comparison t) => t -> t -> t -> t -> Bool
propComparisonTransitiveLE _ e1 e2 e3 = transitive (<=) e1 e2 e3
    
propExtremaInComparison :: (Comparison t, HasExtrema t) => t -> t -> Bool
propExtremaInComparison _ = extrema (<=) least highest

testsComparison ::
    (Comparison t,
     HasExtrema t,
     Arbitrary t,
     ArbitraryOrderedTuple t) =>
    (String, t) -> (Maybe (String, t)) -> Test
testsComparison (name, sample) maybeIllegalArg =
    testGroup (name ++ " (>=)") $ 
        (case maybeIllegalArg of 
            Nothing -> []
            Just (illegalArgName, illegalArg) -> 
                [testProperty (illegalArgName ++ " exception") 
                              (propComparisonIllegalArgException illegalArg)]) 
        ++
        [
         testProperty "anti symmetric" (propComparisonAntiSymmetric sample)
        ,
         testProperty "transitive EQ" (propComparisonTransitiveEQ sample)
        ,
         testProperty "transitive LT" (propComparisonTransitiveLT sample)
        ,
         testProperty "transitive LE" (propComparisonTransitiveLE sample)
        ,
         testProperty "extrema" (propExtremaInComparison sample)
        ]

