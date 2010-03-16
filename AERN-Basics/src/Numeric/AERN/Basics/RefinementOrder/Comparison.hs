{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.Comparison
    Description :  partially ordered sets using refinement order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Partially ordered sets using refinement order notation.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.Comparison where

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.Extrema
import Numeric.AERN.Basics.Laws.Relation

import Numeric.AERN.Misc.Bool

import qualified Prelude 
import Prelude hiding (Eq, (==), compare, EQ, LT, GT)
import Test.QuickCheck


{-|
    A partially ordered set.
    
    (More-or-less copied from Data.Comparison 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class Comparison t where
    compare :: t -> t -> PartialOrdering
    
    -- | non-reflexive inequality
    (|==)  :: t -> t -> Bool
    -- | Is comparable to.
    (|<==>)  :: t -> t -> Bool
    -- | Is not comparable to.
    (|</=>)  :: t -> t -> Bool
    (|<)     :: t -> t -> Bool
    (|<=)    :: t -> t -> Bool
    (|>=)    :: t -> t -> Bool
    (|>)     :: t -> t -> Bool

    -- defaults for all but compare:
    a |==   b = a `compare` b Prelude.== EQ
    a |<    b = a `compare` b Prelude.== LT
    a |>    b = a `compare` b Prelude.== GT
    a |<==> b = a `compare` b Prelude./= NC
    a |</=> b = a `compare` b Prelude.== NC
    a |<=   b = a `compare` b `elem` [EQ, LT, LE]
    a |>=   b = a `compare` b `elem` [EQ, GT, GE]

-- convenience Unicode math operator notation:
(⊏) :: (Comparison t) => t -> t -> Bool
(⊏) = (|<)
(⊑) :: (Comparison t) => t -> t -> Bool
(⊑) = (|<=)
(⊒) :: (Comparison t) => t -> t -> Bool
(⊒) = (|>=)
(⊐) :: (Comparison t) => t -> t -> Bool
(⊐) = (|>)

propComparisonIllegalArgException :: (Comparison t) => t -> t -> Bool
propComparisonIllegalArgException illegalArg e =
    and $ map raisesAERNException 
                [compare e illegalArg, compare illegalArg e]

propComparisonAntiSymmetric :: (Comparison t) => t -> t -> Bool
propComparisonAntiSymmetric e1 e2 = 
    compare e2 e1 Prelude.== (partialOrderingTranspose $ compare e1 e2) 

propComparisonTransitiveEQ :: (Comparison t) => t -> t -> t -> Bool
propComparisonTransitiveEQ = transitive (|==)
    
propComparisonTransitiveLT :: (Comparison t) => t -> t -> t -> Bool
propComparisonTransitiveLT = transitive (⊏)
    
propComparisonTransitiveLE :: (Comparison t) => t -> t -> t -> Bool
propComparisonTransitiveLE = transitive (⊑)
    
propExtremaInComparison :: (Comparison t, HasExtrema t) => t -> Bool
propExtremaInComparison = extrema (⊑) (⊥) (⊤)
    