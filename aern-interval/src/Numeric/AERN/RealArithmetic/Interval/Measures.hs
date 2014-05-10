{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Numeric.AERN.RealArithmetic.Interval.Effort

import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Consistency

import qualified 
       Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified 
       Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import qualified 
       Numeric.AERN.RefinementOrder as RefOrd


instance 
    (ArithUpDn.RoundedReal e,
     ArithInOut.RoundedField (Distance e),
     RefOrd.RoundedLattice (Distance e)) 
    => 
    HasDistance (Interval e) 
    where
    type Distance (Interval e) = Distance e
    type DistanceEffortIndicator (Interval e) =
        IntervalRealEffort e
    distanceDefaultEffort i =
        defaultIntervalRealEffort i
    distanceBetweenEff effort (Interval l1 r1) (Interval l2 r2) =
        distL <+> distR
        where
        distL = distanceBetweenEff effortDist l1 l2
        distR = distanceBetweenEff effortDist r1 r2
        (<+>) = ArithInOut.addOutEff effortAdd
        effortAdd = ArithInOut.fldEffortAdd sampleDist effDistField
        sampleDist = distL
        effDistField = intrealeff_distField effort 
        effortDist = ArithUpDn.rrEffortDistance sampleE effE
        effE = intrealeff_eRoundedReal effort 
        sampleE = l1
    
instance
    (ArithUpDn.RoundedReal e,
     Neg (Distance e), 
     ArithInOut.RoundedField (Distance e),
     RefOrd.RoundedLattice (Distance e)) 
    => 
    HasImprecision (Interval e) 
    where
    type Imprecision (Interval e) = Distance e
    type ImprecisionEffortIndicator (Interval e) = 
        IntervalRealEffort e
    imprecisionDefaultEffort i =
        defaultIntervalRealEffort i 
    imprecisionOfEff effort i@(Interval l r) =
        case (isConsistentEff effortOrd i) of
            Just True -> dist
            Just False -> neg dist
            Nothing -> dist <⊓> (neg dist)
        where
        (<⊓>) = RefOrd.meetOutEff effortMeet
        dist = distanceBetweenEff effortDist l r
        
        effortOrd = intrealeff_intordeff sampleE effort
        effortMeet = intrealeff_distJoinMeet effort
        effortDist = ArithUpDn.rrEffortDistance l effE
        effE = intrealeff_eRoundedReal effort
        sampleE = l
        
    
