{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps where

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

class RoundedAdd t where
    type AddEffortIndicator t
    addInEff :: AddEffortIndicator t -> t -> t -> t
    addOutEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t

class (RoundedAdd t, Neg t) => RoundedSubtr t where
    subtrInEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrOutEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrInEff effort a b = addInEff effort a (neg b)
    subtrOutEff effort a b = addOutEff effort a (neg b)
