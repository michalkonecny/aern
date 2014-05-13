{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Effort
    Description :  composite effort indicator for basic interval real operations 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Composite effort indicator for basic interval real operations.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.Effort 
where

import Numeric.AERN.Basics.Interval

import qualified 
       Numeric.AERN.NumericOrder as NumOrd 
import qualified 
       Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.RealArithmetic.Measures (Distance, distanceBetween)

import qualified 
       Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
       (RoundedReal, RoundedRealEffortIndicator(..), roundedRealDefaultEffort)
import qualified 
       Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
       (RoundedFieldEffort, FieldOpsEffortIndicator, fieldOpsDefaultEffort)

import Numeric.AERN.Basics.Effort

import Test.QuickCheck (Arbitrary) --, arbitrary, vectorOf)

data IntervalRealEffort e =
    IntervalRealEffort
    {
        intrealeff_eRoundedReal :: ArithUpDn.RoundedRealEffortIndicator e,
        intrealeff_distField :: ArithInOut.FieldOpsEffortIndicator (Distance e),       
        intrealeff_distComp :: NumOrd.PartialCompareEffortIndicator (Distance e),
        intrealeff_distJoinMeet :: RefOrd.JoinMeetEffortIndicator (Distance e)
    }

-- TODO: complete the following instances:
instance Arbitrary (IntervalRealEffort e)
instance Show (IntervalRealEffort e)
instance EffortIndicator (IntervalRealEffort e)

intrealeff_intordeff ::
    (ArithUpDn.RoundedReal e)
    => 
    e -> IntervalRealEffort e -> IntervalOrderEffort e
intrealeff_intordeff sampleE intrealeff =
    IntervalOrderEffort
    {
        intordeff_eComp = ArithUpDn.rrEffortComp sampleE effE,
        intordeff_eMinmax = ArithUpDn.rrEffortMinmax sampleE effE 
    }
    where
    effE = intrealeff_eRoundedReal intrealeff

intrealeff_eComp ::
    (ArithUpDn.RoundedReal e)
    => 
    e -> IntervalRealEffort e -> NumOrd.PartialCompareEffortIndicator e
intrealeff_eComp sampleE intrealeff =
    intordeff_eComp $
        intrealeff_intordeff sampleE intrealeff

intrealeff_eMinmax ::
    (ArithUpDn.RoundedReal e)
    => 
    e -> IntervalRealEffort e -> NumOrd.MinmaxEffortIndicator e
intrealeff_eMinmax sampleE intrealeff =
    intordeff_eMinmax $
        intrealeff_intordeff sampleE intrealeff

defaultIntervalRealEffort :: 
   (ArithUpDn.RoundedReal e,
    RefOrd.RoundedLatticeEffort (Distance e),
    ArithInOut.RoundedFieldEffort (Distance e)) 
   =>
   Interval e -> IntervalRealEffort e
defaultIntervalRealEffort (Interval sampleE _) =
    IntervalRealEffort
    {
        intrealeff_eRoundedReal = ArithUpDn.roundedRealDefaultEffort sampleE,
        intrealeff_distField = ArithInOut.fieldOpsDefaultEffort sampleDist,         
        intrealeff_distComp = NumOrd.pCompareDefaultEffort sampleDist,
        intrealeff_distJoinMeet = RefOrd.joinmeetDefaultEffort sampleDist
    }
    where
    sampleDist = distanceBetween sampleE sampleE
    