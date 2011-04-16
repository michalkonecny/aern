{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort
    Description :  convenience in-place operators and functions with default effort  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience in-place operators and functions with default effort.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

-- TODO move to Numeric.AERN.Basics.Mutable
pureEffToMutable1 ::
    (CanBeMutable t) =>
    (eff -> t -> t) ->
    (t -> eff) ->
    OpMutable1 t s
pureEffToMutable1 pureEffFn defEff resM aM =
    do
    d <- readMutable resM
    res <- readMutable resM
    a <- readMutable aM
    unsafeWriteMutable resM (pureEffFn (defEff d) a)

-- TODO move to Numeric.AERN.Basics.Mutable
pureEffToMutable2 ::
    (CanBeMutable t) =>
    (eff -> t -> t -> t) ->
    (t -> eff) ->
    OpMutable2 t s
pureEffToMutable2 pureEffFn defEff resM aM bM =
    do
    d <- readMutable resM
    res <- readMutable resM
    a <- readMutable aM
    b <- readMutable bM
    unsafeWriteMutable resM (pureEffFn (defEff d) a b)

-- TODO move to Numeric.AERN.Basics.Mutable
pureEffToMutableNonmut ::
    (CanBeMutable t) =>
    (eff -> t -> nonmut -> t) ->
    (t -> eff) ->
    OpMutableNonmut t nonmut s
pureEffToMutableNonmut pureEffFn defEff resM aM b =
    do
    d <- readMutable resM
    res <- readMutable resM
    a <- readMutable aM
    unsafeWriteMutable resM (pureEffFn (defEff d) a b)

mutable2ToMutable1 ::
    (CanBeMutable t) =>
    OpMutable2 t s -> OpMutable1 t s
mutable2ToMutable1 mutOp aM bM =
    mutOp aM aM bM

---- | Inward rounded in place addition
--addInInPlace :: (RoundedAddInPlace t) => OpMutable2 t s
--addInInPlace resM lM rM = 
--  do
--  res <- readMutable resM 
--  addInInPlaceEff (addDefaultEffort res) resM lM rM 

-- | Inward rounded in place addition
addInInPlace :: (CanBeMutable t, RoundedAdd t) => OpMutable2 t s
addInInPlace = pureEffToMutable2 addInEff addDefaultEffort

-- associated assignment operator

-- | Inward rounded addition assignment
(>+<=) :: (CanBeMutable t, RoundedAdd t) => OpMutable1 t s
(>+<=) = mutable2ToMutable1 addInInPlace

-- | Outward rounded in place addition
addOutInPlace :: (CanBeMutable t, RoundedAdd t) => OpMutable2 t s
addOutInPlace = pureEffToMutable2 addOutEff addDefaultEffort 

-- | Outward rounded addition assignment
(<+>=) :: (CanBeMutable t, RoundedAdd t) => OpMutable1 t s
(<+>=) = mutable2ToMutable1 addOutInPlace

-- | Inward rounded in place subtraction
subtrInInPlace :: (CanBeMutable t, RoundedSubtr t) => OpMutable2 t s
subtrInInPlace = pureEffToMutable2 subtrInEff addDefaultEffort

-- | Inward rounded subtraction assignment
(>-<=) :: (CanBeMutable t, RoundedSubtr t) => OpMutable1 t s
(>-<=) = mutable2ToMutable1 subtrInInPlace

-- | Outward rounded in place subtraction
subtrOutInPlace :: (CanBeMutable t, RoundedSubtr t) => OpMutable2 t s
subtrOutInPlace = pureEffToMutable2 subtrOutEff addDefaultEffort

-- | Outward rounded subtraction assignment
(<->=) :: (CanBeMutable t, RoundedSubtr t) => OpMutable1 t s
(<->=) = mutable2ToMutable1 subtrOutInPlace

-- | Inward rounded in place absolute value
absInInPlace :: (CanBeMutable t, RoundedAbs t) => OpMutable1 t s
absInInPlace = pureEffToMutable1 absInEff absDefaultEffort 

-- | Outward rounded in place absolute value
absOutInPlace :: (CanBeMutable t, RoundedAbs t) => OpMutable1 t s
absOutInPlace = pureEffToMutable1 absOutEff absDefaultEffort 

-- | Inward rounded in place multiplication
multInInPlace :: (CanBeMutable t, RoundedMultiply t) => OpMutable2 t s
multInInPlace = pureEffToMutable2 multInEff multDefaultEffort

-- | Inward rounded multiplication assignment
(>*<=) :: (CanBeMutable t, RoundedMultiply t) => OpMutable1 t s
(>*<=) = mutable2ToMutable1 multInInPlace

-- | Outward rounded in place multiplication
multOutInPlace :: (CanBeMutable t, RoundedMultiply t) => OpMutable2 t s
multOutInPlace = pureEffToMutable2 multOutEff multDefaultEffort

-- | Outward rounded multiplication assignment
(<*>=) :: (CanBeMutable t, RoundedMultiply t) => OpMutable1 t s
(<*>=) = mutable2ToMutable1 multOutInPlace

-- | Inward rounded in place power
powerToNonnegIntInInPlace :: (CanBeMutable t, RoundedPowerToNonnegInt t) => 
    OpMutableNonmut t Int s
powerToNonnegIntInInPlace = 
    pureEffToMutableNonmut powerToNonnegIntInEff powerToNonnegIntDefaultEffort

-- | Outward rounded in place power
powerToNonnegIntOutInPlace :: (CanBeMutable t, RoundedPowerToNonnegInt t) => 
    OpMutableNonmut t Int s
powerToNonnegIntOutInPlace = 
    pureEffToMutableNonmut powerToNonnegIntOutEff powerToNonnegIntDefaultEffort

-- | Inward rounded in place division
divInInPlace :: (CanBeMutable t, RoundedDivide t) => OpMutable2 t s
divInInPlace = pureEffToMutable2 divInEff divDefaultEffort

-- | Inward rounded division assignment
(>/<=) :: (CanBeMutable t, RoundedDivide t) => OpMutable1 t s
(>/<=) = mutable2ToMutable1 divInInPlace

-- | Outward rounded in place division
divOutInPlace :: (CanBeMutable t, RoundedDivide t) => OpMutable2 t s
divOutInPlace = pureEffToMutable2 divOutEff divDefaultEffort

-- | Outward rounded division assignment
(</>=) :: (CanBeMutable t, RoundedDivide t) => OpMutable1 t s
(</>=) = mutable2ToMutable1 divOutInPlace

-- | Inward rounded in place additive scalar left action

-- | Outward rounded in place additive scalar left action

-- | Inward rounded in place additive scalar right action

-- | Outward rounded in place additive scalar right action

-- | Inward rounded in place multiplicative scalar left action

-- | Outward rounded in place multiplicative scalar left action

-- | Inward rounded in place multiplicative scalar right action

-- | Outward rounded in place multiplicative scalar right action

-- | Inward rounded in place multiplicative scalar reciprocal right action

-- | Outward rounded in place multiplicative scalar reciprocal right action


