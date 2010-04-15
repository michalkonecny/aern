{-# LANGUAGE TypeFamilies #-}

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
    (+^) :: t -> t -> t
    (+.) :: t -> t -> t
    a +^ b = addUpEff (addDefaultEffort a) a b
    a +. b = addDnEff (addDefaultEffort a) a b

class Neg t where
    neg :: t -> t

class (RoundedAdd t, Neg t) => RoundedSubtr t where
    (-^) :: t -> t -> t
    (-.) :: t -> t -> t
    a -^ b = addUpEff (addDefaultEffort a) a (neg b)
    a -. b = addDnEff (addDefaultEffort a) a (neg b)
