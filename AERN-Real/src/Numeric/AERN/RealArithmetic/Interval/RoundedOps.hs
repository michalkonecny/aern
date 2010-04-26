{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.RoundedOps
    Description :  refinement rounded basic operations for intervals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Refinement rounded basic operations for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.RoundedOps where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.Interval.ExactOperations

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd


instance (ArithUpDn.RoundedAdd e) => RoundedAdd (Interval e) where
    type AddEffortIndicator (Interval e) = ArithUpDn.AddEffortIndicator e
    addDefaultEffort (Interval l h) = ArithUpDn.addDefaultEffort l
    addInEff effort (Interval l1 h1) (Interval l2 h2) =
        Interval 
            (ArithUpDn.addUpEff effort l1 l2)
            (ArithUpDn.addDnEff effort h1 h2)
    addOutEff effort (Interval l1 h1) (Interval l2 h2) =
        Interval 
            (ArithUpDn.addDnEff effort l1 l2)
            (ArithUpDn.addUpEff effort h1 h2)

instance (ArithUpDn.RoundedAdd e, Neg e) => RoundedSubtr (Interval e)

instance 
    (ArithUpDn.RoundedAbs e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedAbs (Interval e)
    where
    type AbsEffortIndicator (Interval e) = 
        (NumOrd.PartialCompareEffortIndicator e, NumOrd.MinmaxEffortIndicator e)
    absDefaultEffort (Interval l h) = 
        (NumOrd.pCompareDefaultEffort l, NumOrd.minmaxDefaultEffort l) 
    absOutEff = absOutUsingCompMax
    absInEff = absInUsingCompMax

-- TODO: multiplication and division
            