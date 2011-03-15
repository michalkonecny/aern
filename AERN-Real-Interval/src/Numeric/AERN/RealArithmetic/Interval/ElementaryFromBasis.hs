{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis
    Description :  elementary operations using basis-level operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary operations using basis-level operations.
-}

module Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis where

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Interval

instance (ArithUpDn.RoundedExponentiationEffort e) => 
    (ArithInOut.RoundedExponentiationEffort (Interval e)) 
    where
    type ArithInOut.ExpEffortIndicator (Interval e) = ArithUpDn.ExpEffortIndicator e
    expDefaultEffort (Interval l h) = ArithUpDn.expDefaultEffort l

instance (ArithUpDn.RoundedExponentiation e) => 
    (ArithInOut.RoundedExponentiation (Interval e)) 
    where
    expInEff effort (Interval l h) =
        Interval (ArithUpDn.expUpEff effort l) (ArithUpDn.expDnEff effort h)
    expOutEff effort (Interval l h) =
        Interval (ArithUpDn.expDnEff effort l) (ArithUpDn.expUpEff effort h)

instance (ArithUpDn.RoundedSquareRootEffort e) => 
    (ArithInOut.RoundedSquareRootEffort (Interval e)) 
    where
    type ArithInOut.SqrtEffortIndicator (Interval e) = ArithUpDn.SqrtEffortIndicator e
    sqrtDefaultEffort (Interval l h) = ArithUpDn.sqrtDefaultEffort l

instance (ArithUpDn.RoundedSquareRoot e) => 
    (ArithInOut.RoundedSquareRoot (Interval e)) 
    where
    sqrtInEff effort (Interval l h) =
        Interval (ArithUpDn.sqrtUpEff effort l) (ArithUpDn.sqrtDnEff effort h)
    sqrtOutEff effort (Interval l h) =
        Interval (ArithUpDn.sqrtDnEff effort l) (ArithUpDn.sqrtUpEff effort h)
        