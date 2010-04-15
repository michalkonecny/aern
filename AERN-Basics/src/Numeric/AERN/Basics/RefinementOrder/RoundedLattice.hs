{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.RoundedLattice
    Description :  lattices with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with outwards and inwards rounded operations.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}
module Numeric.AERN.Basics.RefinementOrder.RoundedLattice 
where

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.Arbitrary
import Numeric.AERN.Basics.RefinementOrder.PartialComparison

import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

infixr 3 <|/\>, <⊓>, >|/\<, >⊓< 
infixr 2 <|\/>, <⊔>, >|\/<, >⊔< 

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedLattice t where
    type JoinMeetOutEffortIndicator t
    joinmeetOutDefaultEffort :: t -> JoinMeetOutEffortIndicator t

    joinOut :: JoinMeetOutEffortIndicator t -> t -> t -> t
    meetOut :: JoinMeetOutEffortIndicator t -> t -> t -> t

    (<|\/>) :: t -> t -> t
    (<|/\>) :: t -> t -> t
    
    a <|\/> b = joinOut (joinmeetOutDefaultEffort a) a b 
    a <|/\> b = meetOut (joinmeetOutDefaultEffort a) a b 

{-| convenience Unicode notation for '<|\/>' -}
(<⊔>) :: (OuterRoundedLattice t) => t -> t -> t
(<⊔>) = (<|\/>)
{-| convenience Unicode notation for '<|/\>' -}
(<⊓>) :: (OuterRoundedLattice t) => t -> t -> t
(<⊓>) = (<|/\>)


{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedLattice t where
    type JoinMeetInEffortIndicator t
    joinmeetInDefaultEffort :: t -> JoinMeetInEffortIndicator t
    
    joinIn :: JoinMeetInEffortIndicator t -> t -> t -> t
    meetIn :: JoinMeetInEffortIndicator t -> t -> t -> t

    (>|\/<) :: t -> t -> t
    (>|/\<) :: t -> t -> t
    
    a >|\/< b = joinIn (joinmeetInDefaultEffort a) a b 
    a >|/\< b = meetIn (joinmeetInDefaultEffort a) a b 

{-| convenience Unicode notation for '>|\/<' -}
(>⊔<) :: (InnerRoundedLattice t) => t -> t -> t
(>⊔<) = (>|\/<)
{-| convenience Unicode notation for '>|/\<' -}
(>⊓<) :: (InnerRoundedLattice t) => t -> t -> t
(>⊓<) = (>|/\<)

class (InnerRoundedLattice t, OuterRoundedLattice t) => RoundedLattice t

-- properties of RoundedLattice

propRoundedLatticeComparisonCompatible :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible _ (UniformlyOrderedPair (e1,e2)) = 
    (downRoundedJoinOfOrderedPair (|<=?) (<⊓>) e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (|<=?) (>⊔<) e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth _ (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (|<=?) (>⊔<) e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth _ (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (|<=?) (<⊓>) e1 e2

propRoundedLatticeJoinIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> t -> Bool
propRoundedLatticeJoinIdempotent _ = 
    roundedIdempotent (|<=?) (>⊔<) (<⊔>)

propRoundedLatticeJoinCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (|<=?) (>⊔<) (<⊔>) e1 e2

propRoundedLatticeJoinAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (|<=?) (>⊔<) (<⊔>) e1 e2 e3

propRoundedLatticeMeetIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> t -> Bool
propRoundedLatticeMeetIdempotent _ = 
    roundedIdempotent (|<=?) (>⊓<) (<⊓>)

propRoundedLatticeMeetCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (|<=?) (>⊓<) (<⊓>) e1 e2

propRoundedLatticeMeetAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (|<=?) (>⊓<) (<⊓>) e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3

propRoundedLatticeDistributive :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3)
    && 
    (roundedLeftDistributive  (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3)


testsRoundedLatticeDistributive :: 
    (PartialComparison t,
     RoundedLattice t,
     Arbitrary t, 
     ArbitraryOrderedTuple t,
     Eq t, 
     Show t) => 
    (String, t) -> Test
testsRoundedLatticeDistributive (name, sample) =
    testGroup (name ++ " (min,max) rounded") $
        [
         testProperty "Comparison compatible" (propRoundedLatticeComparisonCompatible sample)
        ,
         testProperty "join above" (propRoundedLatticeJoinAboveBoth sample)
        ,
         testProperty "meet below" (propRoundedLatticeMeetBelowBoth sample)
        ,
         testProperty "join idempotent" (propRoundedLatticeJoinIdempotent sample)
        ,
         testProperty "join commutative" (propRoundedLatticeJoinCommutative sample)
        ,
         testProperty "join associative" (propRoundedLatticeJoinAssocative sample)
        ,
         testProperty "meet idempotent" (propRoundedLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propRoundedLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propRoundedLatticeMeetAssocative sample)
        ,
         testProperty "distributive" (propRoundedLatticeDistributive sample)
        ]
    
-- mutable versions (TODO)    