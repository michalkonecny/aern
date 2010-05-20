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

instance (ArithUpDn.RoundedExponentiation e) => (ArithInOut.RoundedExponentiation (Interval e)) where
    type ArithInOut.ExpEffortIndicator (Interval e) = ArithUpDn.ExpEffortIndicator e
    expDefaultEffortIndicator (Interval l h) = ArithUpDn.expDefaultEffortIndicator l
    expInEff effort (Interval l h) =
        Interval (ArithUpDn.expUpEff effort l) (ArithUpDn.expDnEff effort h)
    expOutEff effort (Interval l h) =
        Interval (ArithUpDn.expDnEff effort l) (ArithUpDn.expUpEff effort h)
