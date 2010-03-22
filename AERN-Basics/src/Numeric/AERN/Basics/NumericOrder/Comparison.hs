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

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.NumericOrder.SemidecidableComparison
import Numeric.AERN.Basics.Laws.Relation

import Numeric.AERN.Misc.Bool

import qualified Prelude
import Prelude hiding (Eq, (==), compare, EQ, LT, GT, (<), (<=), (>=), (>))


{-|
    A partially ordered set.
    
    (More-or-less copied from Data.Comparison 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class (SemidecidableComparison t, Show t) => Comparison t where
    compare :: t -> t -> PartialOrdering
    
    -- default implementation assuming the inherited semidecidable order is actually decidable:
    compare a b =
        case maybeCompare a b of
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

propComparisonIllegalArgException :: (Comparison t) => t -> t -> Bool
propComparisonIllegalArgException illegalArg e =
    and $ map raisesAERNException 
                [compare e illegalArg, compare illegalArg e] 

propComparisonReflexiveEQ :: (Comparison t) => t -> Bool
propComparisonReflexiveEQ e = 
    compare e e Prelude.== EQ 

propComparisonAntiSymmetric :: (Comparison t) => UniformlyOrderedPair t -> Bool
propComparisonAntiSymmetric (UniformlyOrderedPair (e1, e2)) = 
    compare e2 e1 Prelude.== (partialOrderingTranspose $ compare e1 e2) 

propComparisonTransitiveEQ :: (Comparison t) => t -> t -> t -> Bool
propComparisonTransitiveEQ e1 e2 e3 = transitive (==) e1 e2 e3
    
propComparisonTransitiveLT :: (Comparison t) => t -> t -> t -> Bool
propComparisonTransitiveLT e1 e2 e3 = transitive (<) e1 e2 e3
    
propComparisonTransitiveLE :: (Comparison t) => t -> t -> t -> Bool
propComparisonTransitiveLE e1 e2 e3 = transitive (<=) e1 e2 e3
    
propExtremaInComparison :: (Comparison t, HasExtrema t) => t -> Bool
propExtremaInComparison = extrema (<=) least highest

