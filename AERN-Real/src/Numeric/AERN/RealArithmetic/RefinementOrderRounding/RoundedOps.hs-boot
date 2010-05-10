{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps where

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Numerals

import Numeric.AERN.Basics.Effort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

infixl 6 <+>, >+<, <->, >-<
-- infixl 7 <*>, >*<

class RoundedAdd t where
    type AddEffortIndicator t
    addInEff :: AddEffortIndicator t -> t -> t -> t
    addOutEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t
    (>+<) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    (<+>) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    (>+<) = addInEff ?addInOutEffort
    (<+>) = addOutEff ?addInOutEffort

class (RoundedAdd t, Neg t) => RoundedSubtr t where
    subtrInEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrOutEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrInEff effort a b = addInEff effort a (neg b)
    subtrOutEff effort a b = addOutEff effort a (neg b)
    (>-<) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    (<->) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    a >-< b = addInEff ?addInOutEffort a (neg b)
    a <-> b = addOutEff ?addInOutEffort a (neg b)
