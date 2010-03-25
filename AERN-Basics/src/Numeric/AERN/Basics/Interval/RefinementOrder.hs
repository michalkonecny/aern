{-|
    Module      :  Numeric.AERN.Basics.Interval.RefinementOrder
    Description :  interval instances of refinement-ordered structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval instances of refinement-ordered structures.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.RefinementOrder where

import Numeric.AERN.Basics.Interval.Basics
import Numeric.AERN.Basics.Interval.NumericOrder
import Numeric.AERN.Basics.CInterval.RefinementOrder

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

instance (NumOrd.Comparison e) => (RefOrd.Comparison (Interval e))
    where
    compare = compareIntervalRef
    
instance (NumOrd.SemidecidableComparison e) => (RefOrd.SemidecidableComparison (Interval e))
    where
    maybeCompareEff = maybeCompareEffIntervalRef
    maybeCompareDefaultEffort = maybeCompareDefaultEffortIntervalRef

instance (NumOrd.HasExtrema e) => (RefOrd.HasTop (Interval e))
    where
    top = topInterval
    
instance (NumOrd.HasExtrema e) => (RefOrd.HasBottom (Interval e))
    where
    bottom = bottomInterval

instance (NumOrd.HasExtrema e) => (RefOrd.HasExtrema (Interval e))

instance (NumOrd.Lattice e, NumOrd.SemidecidableComparison e) => (RefOrd.Basis (Interval e)) 
    where
    (|\/?) = basisJoinInterval

instance (NumOrd.RoundedLattice e, NumOrd.SemidecidableComparison e) => (RefOrd.OuterRoundedBasis (Interval e)) 
    where
    partialJoinOut = outerRoundedPartialJoinInterval
    partialJoinOutDefaultEffort = joinmeetDefaultEffortInterval

instance (NumOrd.RoundedLattice e, NumOrd.SemidecidableComparison e) => (RefOrd.InnerRoundedBasis (Interval e)) 
    where
    partialJoinIn = innerRoundedPartialJoinInterval
    partialJoinInDefaultEffort = joinmeetDefaultEffortInterval

instance (NumOrd.RoundedLattice e, NumOrd.SemidecidableComparison e) => (RefOrd.RoundedBasis (Interval e)) 


instance (NumOrd.Lattice e, NumOrd.SemidecidableComparison e) => (RefOrd.Lattice (Interval e)) 
    where
    (|\/) = joinInterval
    (|/\) = meetInterval

instance (NumOrd.RoundedLattice e, NumOrd.SemidecidableComparison e) => (RefOrd.OuterRoundedLattice (Interval e)) 
    where
    joinOut = outerRoundedJoinInterval
    meetOut = outerRoundedMeetInterval
    joinmeetOutDefaultEffort = joinmeetDefaultEffortInterval

instance (NumOrd.RoundedLattice e, NumOrd.SemidecidableComparison e) => (RefOrd.InnerRoundedLattice (Interval e)) 
    where
    joinIn = innerRoundedJoinInterval
    meetIn = innerRoundedMeetInterval
    joinmeetInDefaultEffort = joinmeetDefaultEffortInterval

instance (NumOrd.RoundedLattice e, NumOrd.SemidecidableComparison e) => (RefOrd.RoundedLattice (Interval e))

instance (NumOrd.ArbitraryOrderedTuple e) => RefOrd.ArbitraryOrderedTuple (Interval e) where
   arbitraryTupleRelatedBy = arbitraryIntervalTupleRefinementRelatedBy
       
       
       
    