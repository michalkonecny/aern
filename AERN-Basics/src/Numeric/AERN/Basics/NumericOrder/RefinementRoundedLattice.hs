{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice
    Description :  lattices over numerical order but with refinement order rounding  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices over numerical order but with refinement order rounding.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Arbitrary 
import Numeric.AERN.Basics.NumericOrder.SemidecidableComparison 
import Numeric.AERN.Basics.NumericOrder.Extrema

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder ((|<=?))

import Numeric.AERN.Basics.Laws.SemidecidableRelation
import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with refinement-outer-rounding numerical-order-lattice operations.
-}
class OuterRoundedLattice t where
    type MinmaxOuterEffortIndicator t
    minmaxOuterDefaultEffort :: t -> MinmaxOuterEffortIndicator t

    maxOuterEff :: MinmaxOuterEffortIndicator t -> t -> t -> t
    minOuterEff :: MinmaxOuterEffortIndicator t -> t -> t -> t

    maxOuter :: t -> t -> t
    minOuter :: t -> t -> t
    
    maxOuter a b = maxOuterEff (minmaxOuterDefaultEffort a) a b 
    minOuter a b = minOuterEff (minmaxOuterDefaultEffort a) a b 

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class InnerRoundedLattice t where
    type MinmaxInnerEffortIndicator t
    minmaxInnerDefaultEffort :: t -> MinmaxInnerEffortIndicator t

    maxInnerEff :: MinmaxInnerEffortIndicator t -> t -> t -> t
    minInnerEff :: MinmaxInnerEffortIndicator t -> t -> t -> t

    maxInner :: t -> t -> t
    minInner :: t -> t -> t
    
    maxInner a b = maxInnerEff (minmaxInnerDefaultEffort a) a b 
    minInner a b = minInnerEff (minmaxInnerDefaultEffort a) a b 


class (OuterRoundedLattice t, InnerRoundedLattice t) => RefinementRoundedLattice t

propRefinementRoundedLatticeIllegalArgException :: 
    (RefinementRoundedLattice t) => 
    t -> t -> Bool
propRefinementRoundedLatticeIllegalArgException illegalArg d =
    and $ map raisesAERNException $ 
                concat [[op d illegalArg, op illegalArg d] | op <- [maxInner, maxOuter, minInner, minOuter]] 

propRefinementRoundedLatticeJoinIdempotent :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> t -> Bool
propRefinementRoundedLatticeJoinIdempotent _ = roundedIdempotent (|<=?) maxInner maxOuter

propRefinementRoundedLatticeJoinCommutative :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (|<=?) maxInner maxOuter e1 e2

propRefinementRoundedLatticeJoinAssocative :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeJoinAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (|<=?) maxInner maxOuter e1 e2 e3

propRefinementRoundedLatticeMeetIdempotent :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> t -> Bool
propRefinementRoundedLatticeMeetIdempotent _ = 
    roundedIdempotent (|<=?) minInner minOuter

propRefinementRoundedLatticeMeetCommutative :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (|<=?) minInner minOuter e1 e2

propRefinementRoundedLatticeMeetAssocative :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeMeetAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (|<=?) minInner minOuter e1 e2 e3

{- optional properties: -}
propRefinementRoundedLatticeModular :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeModular _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (|<=?) maxInner minInner maxOuter minOuter e1 e2 e3

propRefinementRoundedLatticeDistributive :: 
    (RefOrd.SemidecidableComparison t, RefinementRoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeDistributive _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (|<=?) maxInner minInner maxOuter minOuter e1 e2 e3)
    && 
    (roundedLeftDistributive  (|<=?) maxInner minInner maxOuter minOuter e1 e2 e3)
    
testsRefinementRoundedLatticeDistributive :: 
    (RefOrd.SemidecidableComparison t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, 
     Eq t, 
     Show t) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRefinementRoundedLatticeDistributive (name, sample) maybeIllegalArg =
    testGroup (name ++ " (min,max) treated as refinement rounded") $
        (case maybeIllegalArg of 
            Nothing -> []
            Just (illegalArgName, illegalArg) -> 
                [testProperty (illegalArgName ++ " exception") 
                              (propRefinementRoundedLatticeIllegalArgException illegalArg)]) 
        ++
        [
         testProperty "join idempotent" (propRefinementRoundedLatticeJoinIdempotent sample)
        ,
         testProperty "join commutative" (propRefinementRoundedLatticeJoinCommutative sample)
        ,
         testProperty "join associative" (propRefinementRoundedLatticeJoinAssocative sample)
        ,
         testProperty "meet idempotent" (propRefinementRoundedLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propRefinementRoundedLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propRefinementRoundedLatticeMeetAssocative sample)
        ,
         testProperty "distributive" (propRefinementRoundedLatticeDistributive sample)
        ]
    