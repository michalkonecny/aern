{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.RoundedRing where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Granularity
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

class RoundedAdd t where
    type AddEffortIndicator t
    addUpEff :: AddEffortIndicator t -> t -> t -> t
    addDnEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t
    (+^) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (+.) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (+^) = addUpEff ?addUpDnEffort
    (+.) = addDnEff ?addUpDnEffort

class Neg t where
    neg :: t -> t

class (RoundedAdd t, Neg t) => RoundedSubtr t where
    (-^) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (-.) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    a -^ b = addUpEff ?addUpDnEffort a (neg b)
    a -. b = addDnEff ?addUpDnEffort a (neg b)
