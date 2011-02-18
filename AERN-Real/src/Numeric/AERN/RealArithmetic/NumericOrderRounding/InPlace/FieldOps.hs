{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.FieldOps
    Description :  rounded basic arithmetic operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetic operations.
    
    Each operations takes mutable parameters instead of pure parameters
    and has one extra mutable parameter before the other parameters, 
    in which it stores the result.
    The mutable parameters can alias arbitrarily, making it possible
    to eg add to a number overwriting the original number.
    
    The operations have as their first paramter a non-mutable sample value
    to aid type-checking, ie to help work out which type the mutable parameters
    contain.
    
    This module is hidden and reexported via its parent NumericOrderRounding.InPlace. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.FieldOps 
(
    RoundedAddInPlace(..), 
    RoundedSubtrInPlace(..),
    RoundedAbsInPlace(..),
    RoundedMultiplyInPlace(..),
    RoundedPowerNonnegToNonnegIntInPlace(..),
    powerNonnegToNonnegIntUpInPlaceEffFromMult,
    powerNonnegToNonnegIntDnInPlaceEffFromMult,
    RoundedPowerToNonnegIntInPlace(..), 
    RoundedDivideInPlace(..),
    testsUpDnFieldOpsInPlace,
    RoundedRingInPlace(..), RoundedFieldInPlace(..)
)
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps

import Numeric.AERN.RealArithmetic.Auxiliary
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.ST
import Data.Maybe

class (RoundedAdd t, CanBeMutable t) => RoundedAddInPlace t where
    addUpInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    addDnInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    addUpInPlaceEff sample = pureToMutable2Eff sample addUpEff 
    addDnInPlaceEff sample = pureToMutable2Eff sample addDnEff 

propUpDnAddInPlace ::
    (NumOrd.PartialComparison t, RoundedAddInPlace t, Neg t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> t -> Bool
propUpDnAddInPlace sample initEffort e1 e2 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    addUpEffViaInPlace = mutable2EffToPure (addUpInPlaceEff sample)
    addDnEffViaInPlace = mutable2EffToPure (addDnInPlaceEff sample)
    expr1Up eff =
        let (+^) = addUpEff eff in e1 +^ e2
    expr1Dn eff =
        let (+.) = addDnEff eff in e1 +. e2
    expr2Up eff =
        let (+^) = addUpEffViaInPlace eff in e1 +^ e2
    expr2Dn eff =
        let (+.) = addDnEffViaInPlace eff in e1 +. e2

class (RoundedAddInPlace t,  RoundedSubtr t, NegInPlace t) => RoundedSubtrInPlace t where
    subtrUpInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    subtrDnInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    subtrUpInPlaceEff sample effort rM aM bM =
        do
        bbM <- cloneMutable sample bM
        negInPlace sample bbM bM
        addUpInPlaceEff sample effort rM aM bbM
    subtrDnInPlaceEff sample effort rM aM bM = 
        do
        bbM <- cloneMutable sample bM
        negInPlace sample bbM bM
        addDnInPlaceEff sample effort rM aM bbM

propUpDnSubtrInPlace ::
    (NumOrd.PartialComparison t, RoundedSubtrInPlace t, Neg t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> t -> Bool
propUpDnSubtrInPlace sample initEffort e1 e2 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    subtrUpEffViaInPlace = mutable2EffToPure (subtrUpInPlaceEff sample)
    subtrDnEffViaInPlace = mutable2EffToPure (subtrDnInPlaceEff sample)
    expr1Up eff =
        let (-^) = subtrUpEff eff in e1 -^ e2
    expr1Dn eff =
        let (-.) = subtrDnEff eff in e1 -. e2
    expr2Up eff =
        let (-^) = subtrUpEffViaInPlace eff in e1 -^ e2
    expr2Dn eff =
        let (-.) = subtrDnEffViaInPlace eff in e1 -. e2


class (RoundedAbs t, CanBeMutable t) => RoundedAbsInPlace t where
    absUpInPlaceEff :: t -> OpMutable1Eff (AbsEffortIndicator t) t s
    absDnInPlaceEff :: t -> OpMutable1Eff (AbsEffortIndicator t) t s
    absUpInPlaceEff sample = pureToMutable1Eff sample absUpEff 
    absDnInPlaceEff sample = pureToMutable1Eff sample absDnEff 

propUpDnAbsInPlace ::
    (NumOrd.PartialComparison t, RoundedAbsInPlace t, Neg t,
     Show t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    t -> Bool
propUpDnAbsInPlace sample initEffort e1 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    absUpEffViaInPlace = mutable1EffToPure (absUpInPlaceEff sample)
    absDnEffViaInPlace = mutable1EffToPure (absDnInPlaceEff sample)
    expr1Up eff = absUpEff eff e1
    expr1Dn eff = absDnEff eff e1
    expr2Up eff = absUpEffViaInPlace eff e1
    expr2Dn eff = absDnEffViaInPlace eff e1

class (RoundedMultiply t, CanBeMutable t) => RoundedMultiplyInPlace t where
    multUpInPlaceEff :: t -> OpMutable2Eff (MultEffortIndicator t) t s
    multDnInPlaceEff :: t -> OpMutable2Eff (MultEffortIndicator t) t s
    multUpInPlaceEff sample = pureToMutable2Eff sample multUpEff 
    multDnInPlaceEff sample = pureToMutable2Eff sample multDnEff 

propUpDnMultInPlace ::
    (NumOrd.PartialComparison t, RoundedMultiplyInPlace t, Neg t,
     Show t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    t -> t -> Bool
propUpDnMultInPlace sample initEffort e1 e2 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    multUpEffViaInPlace = mutable2EffToPure (multUpInPlaceEff sample)
    multDnEffViaInPlace = mutable2EffToPure (multDnInPlaceEff sample)
    expr1Up eff =
        let (*^) = multUpEff eff in e1 *^ e2
    expr1Dn eff =
        let (*.) = multDnEff eff in e1 *. e2
    expr2Up eff =
        let (*^) = multUpEffViaInPlace eff in e1 *^ e2
    expr2Dn eff =
        let (*.) = multDnEffViaInPlace eff in e1 *. e2

class (RoundedPowerNonnegToNonnegInt t, CanBeMutable t) => RoundedPowerNonnegToNonnegIntInPlace t where
    powerNonnegToNonnegIntUpInPlaceEff ::
        t -> OpMutableNonmutEff (PowerNonnegToNonnegIntEffortIndicator t) t Int s
    powerNonnegToNonnegIntDnInPlaceEff ::
        t -> OpMutableNonmutEff (PowerNonnegToNonnegIntEffortIndicator t) t Int s
    -- default implementations, do not use these if the RoundedPowerNonnegToNonnegInt
    -- instance uses the ...fromMult implementation; 
    -- in such cases override this implementation with the ...fromMult implementation below
    -- for improved efficiency
    powerNonnegToNonnegIntUpInPlaceEff sample =
        pureToMutableNonmutEff sample powerNonnegToNonnegIntUpEff 
    powerNonnegToNonnegIntDnInPlaceEff sample =
        pureToMutableNonmutEff sample powerNonnegToNonnegIntDnEff 

powerNonnegToNonnegIntUpInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    t ->
    OpMutableNonmutEff (PowerNonnegToNonnegIntEffortIndicatorFromMult t) t Int s 
powerNonnegToNonnegIntUpInPlaceEffFromMult sample effMult rM eM n =
    powerFromMultInPlace sample (multUpInPlaceEff sample effMult) rM eM n

powerNonnegToNonnegIntDnInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    t ->
    OpMutableNonmutEff (PowerNonnegToNonnegIntEffortIndicatorFromMult t) t Int s 
powerNonnegToNonnegIntDnInPlaceEffFromMult sample effMult rM eM n =
    powerFromMultInPlace sample (multDnInPlaceEff sample effMult) rM eM n


class (RoundedPowerToNonnegInt t, CanBeMutable t) => RoundedPowerToNonnegIntInPlace t where
    powerToNonnegIntUpInPlaceEff ::
        t -> OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntDnInPlaceEff ::
        t -> OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntUpInPlaceEff sample =
        pureToMutableNonmutEff sample powerToNonnegIntUpEff 
    powerToNonnegIntDnInPlaceEff sample =
        pureToMutableNonmutEff sample powerToNonnegIntDnEff 

propUpDnPowerToNonnegInPlace ::
    (NumOrd.PartialComparison t, RoundedPowerToNonnegIntInPlace t, Neg t,
     Show t,
     Show (PowerToNonnegIntEffortIndicator t),
     EffortIndicator (PowerToNonnegIntEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     PowerToNonnegIntEffortIndicator t) -> 
    t -> Int -> Bool
propUpDnPowerToNonnegInPlace sample initEffort e1 n =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    powerToNonnegIntUpEffViaInPlace = 
        mutableNonmutEffToPure (powerToNonnegIntUpInPlaceEff sample)
    powerToNonnegIntDnEffViaInPlace = 
        mutableNonmutEffToPure (powerToNonnegIntDnInPlaceEff sample)
    expr1Up eff =
        let (^^) = powerToNonnegIntUpEff eff in e1 ^^ n
    expr1Dn eff =
        let (^.) = powerToNonnegIntDnEff eff in e1 ^. n
    expr2Up eff =
        let (^^) = powerToNonnegIntUpEffViaInPlace eff in e1 ^^ n
    expr2Dn eff =
        let (^.) = powerToNonnegIntDnEffViaInPlace eff in e1 ^. n

class (RoundedDivide t, CanBeMutable t) => RoundedDivideInPlace t where
    divUpInPlaceEff :: t -> OpMutable2Eff (DivEffortIndicator t) t s
    divDnInPlaceEff :: t -> OpMutable2Eff (DivEffortIndicator t) t s
    divUpInPlaceEff sample = pureToMutable2Eff sample divUpEff 
    divDnInPlaceEff sample = pureToMutable2Eff sample divDnEff 

propUpDnDivInPlace ::
    (NumOrd.PartialComparison t, RoundedDivideInPlace t, Neg t,
     Show t, HasZero t,
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     DivEffortIndicator t) -> 
    t -> t -> Bool
propUpDnDivInPlace sample initEffort@(effComp, _) e1 e2 =
    let ?pCompareEffort = effComp in
    case e2 ==? zero of
        Just False ->
            equalRoundingUpDn
                expr1Up expr1Dn expr2Up expr2Dn 
                NumOrd.pLeqEff initEffort
        _ -> True
    where
    divUpEffViaInPlace = mutable2EffToPure (divUpInPlaceEff sample)
    divDnEffViaInPlace = mutable2EffToPure (divDnInPlaceEff sample)
    expr1Up eff =
        let (/^) = divUpEff eff in e1 /^ e2
    expr1Dn eff =
        let (/.) = divDnEff eff in e1 /. e2
    expr2Up eff =
        let (/^) = divUpEffViaInPlace eff in e1 /^ e2
    expr2Dn eff =
        let (/.) = divDnEffViaInPlace eff in e1 /. e2

testsUpDnFieldOpsInPlace (name, sample) =
    testGroup (name ++ " in-place up/down rounded ops match pure ops") $
        [
            testProperty "addition" (propUpDnAddInPlace sample)
        ,
            testProperty "subtraction" (propUpDnSubtrInPlace sample)
        ,
            testProperty "absolute value" (propUpDnAbsInPlace sample)
        ,
            testProperty "multiplication" (propUpDnMultInPlace sample)
        ,
            testProperty "integer power" (propUpDnMultInPlace sample)
        ,
            testProperty "division" (propUpDnDivInPlace sample)
        ]
        

class (RoundedSubtrInPlace t, RoundedMultiplyInPlace t) => RoundedRingInPlace t
class (RoundedRingInPlace t, RoundedDivideInPlace t) => RoundedFieldInPlace t

    