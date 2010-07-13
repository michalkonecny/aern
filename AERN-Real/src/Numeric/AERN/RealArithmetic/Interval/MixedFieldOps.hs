{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing an interval and another type.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.MixedFieldOps where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

instance (ArithUpDn.RoundedMixedAdd t e) => RoundedMixedAdd t (Interval e) where
    type MixedAddEffortIndicator t (Interval e) = ArithUpDn.MixedAddEffortIndicator t e
    mixedAddDefaultEffort t (Interval l h) = ArithUpDn.mixedAddDefaultEffort t l
    mixedAddInEff effort e (Interval l2 h2) =
        Interval
            (ArithUpDn.mixedAddUpEff effort e l2)
            (ArithUpDn.mixedAddDnEff effort e h2)
    mixedAddOutEff effort e (Interval l2 h2) =
        Interval 
            (ArithUpDn.mixedAddDnEff effort e l2)
            (ArithUpDn.mixedAddUpEff effort e h2)

-- TODO: multiplication and division
