{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Measures
    Description :  distance and imprecision for intervals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Distance and imprecision for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.Measures 
()
where

import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Consistency

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort


instance (HasDistance e, ArithInOut.RoundedAdd (Distance e)) => 
    HasDistance (Interval e) where
    type Distance (Interval e) = Distance e
    type DistanceEffortIndicator (Interval e) = 
        (DistanceEffortIndicator e, ArithInOut.AddEffortIndicator (Distance e))
    distanceDefaultEffort (Interval l r) = 
        (effortDist, effortAdd)
        where
        effortDist = distanceDefaultEffort l 
        effortAdd = ArithInOut.addDefaultEffort d 
        d = distanceBetweenEff effortDist l r
    distanceBetweenEff (effortDist, effortAdd) (Interval l1 r1) (Interval l2 r2) =
        let ?addInOutEffort = effortAdd in
        distL <+> distR
        where
        distL = distanceBetweenEff effortDist l1 l2
        distR = distanceBetweenEff effortDist r1 r2
    
instance 
    (HasDistance e, RefOrd.RoundedLattice (Distance e), Neg (Distance e), 
     NumOrd.PartialComparison e) => 
    HasImprecision (Interval e) 
    where
    type Imprecision (Interval e) = Distance e
    type ImprecisionEffortIndicator (Interval e) = 
        (DistanceEffortIndicator e,
         RefOrd.JoinMeetEffortIndicator (Distance e), 
         ConsistencyEffortIndicator (Interval e))
    imprecisionDefaultEffort i@(Interval l r) = 
        (effortDist, effortMeet, consistencyDefaultEffort i) 
        where
        effortDist = distanceDefaultEffort l
        effortMeet = RefOrd.joinmeetDefaultEffort d
        d = distanceBetweenEff effortDist l r
    imprecisionOfEff (effortDist, effortMeet, effortConsistency) i@(Interval l r) =
        let 
        ?joinmeetEffort = effortMeet
        in
        case (isConsistentEff effortConsistency i) of
            Just True -> dist
            Just False -> neg dist
            Nothing -> dist <⊓> (neg dist)
        where 
        dist = distanceBetweenEff effortDist l r
    isExactEff eff@(effortDist, effortMeet, effortConsistency) i =
        case (isConsistentEff effortConsistency i, isAntiConsistentEff effortConsistency i) of
            (Just True, Just True) -> Just True
            (Just False, _) -> Just False
            (_, Just False) -> Just False
            _ -> Nothing
    
