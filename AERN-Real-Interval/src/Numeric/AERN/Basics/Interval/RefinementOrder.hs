{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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

instance 
    (NumOrd.PartialComparison e 
     ) => 
    (RefOrd.PartialComparison (Interval e))
    where
    type RefOrd.PartialCompareEffortIndicator (Interval e) = NumOrd.PartialCompareEffortIndicator e 
    pCompareEff = pCompareEffIntervalRef
    pCompareDefaultEffort = pCompareDefaultEffortIntervalRef

instance (NumOrd.HasExtrema e) => (RefOrd.HasTop (Interval e))
    where
    top = topInterval
    
instance (NumOrd.HasExtrema e) => (RefOrd.HasBottom (Interval e))
    where
    bottom = bottomInterval

instance (NumOrd.HasExtrema e) => (RefOrd.HasExtrema (Interval e))

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.OuterRoundedBasis (Interval e)) 
    where
    type RefOrd.PartialJoinOutEffortIndicator (Interval e) = NumOrd.MinmaxEffortIndicator e 
    partialJoinOutEff = outerRoundedPartialJoinInterval
    partialJoinOutDefaultEffort = joinmeetDefaultEffortInterval

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.InnerRoundedBasis (Interval e)) 
    where
    type RefOrd.PartialJoinInEffortIndicator (Interval e) = NumOrd.MinmaxEffortIndicator e 
    partialJoinInEff = innerRoundedPartialJoinInterval
    partialJoinInDefaultEffort = joinmeetDefaultEffortInterval

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.RoundedBasis (Interval e)) 


instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.OuterRoundedLattice (Interval e)) 
    where
    type RefOrd.JoinMeetOutEffortIndicator (Interval e) = NumOrd.MinmaxEffortIndicator e
    joinOutEff = outerRoundedJoinInterval
    meetOutEff = outerRoundedMeetInterval
    joinmeetOutDefaultEffort = joinmeetDefaultEffortInterval

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.InnerRoundedLattice (Interval e)) 
    where
    type RefOrd.JoinMeetInEffortIndicator (Interval e) = NumOrd.MinmaxEffortIndicator e
    joinInEff = innerRoundedJoinInterval
    meetInEff = innerRoundedMeetInterval
    joinmeetInDefaultEffort = joinmeetDefaultEffortInterval

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.RoundedLattice (Interval e))

instance (NumOrd.ArbitraryOrderedTuple e) => RefOrd.ArbitraryOrderedTuple (Interval e) where
   arbitraryTupleRelatedBy = arbitraryIntervalTupleRefinementRelatedBy
       
       
    