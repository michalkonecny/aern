{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
    Description :  rounded addition and multiplication  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded addition and multiplication.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps 
(
    RoundedAdd(..), RoundedSubtr(..), testsInOutAdd, testsInOutSubtr,
    RoundedAbs(..), testsInOutAbs,  absInUsingCompMax, absOutUsingCompMax,
    RoundedMultiply(..), testsInOutMult,
    RoundedPowerToNonnegInt(..), testsInOutIntPower,
    PowerToNonnegIntEffortIndicatorFromMult, powerToNonnegIntDefaultEffortFromMult,
    powerToNonnegIntInEffFromMult, powerToNonnegIntOutEffFromMult,
    RoundedDivide(..), testsInOutDiv,
    RoundedRing(..), RoundedField(..)
--    ,
--    FieldOpsEffortIndicator(..), fieldOpsDefaultEffort
)
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.Auxiliary
import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe

class RoundedAdd t where
    type AddEffortIndicator t
    addInEff :: AddEffortIndicator t -> t -> t -> t
    addOutEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t

--propAddMonotone _ effortDist

propInOutAddZero ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> Bool
propInOutAddZero _ =
    roundedUnit zero RefOrd.pLeqEff addInEff addOutEff

propInOutAddCommutative ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> t -> Bool
propInOutAddCommutative _ =
    roundedCommutative RefOrd.pLeqEff addInEff addOutEff

propInOutAddAssociative ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> t -> t -> Bool
propInOutAddAssociative _ =
    roundedAssociative RefOrd.pLeqEff addInEff addOutEff

propInOutAddMonotone ::
    (RefOrd.PartialComparison t, RoundedAdd t, Show t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (AddEffortIndicator t) -> 
    (RefOrd.LEPair t) -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutAddMonotone _ =
    roundedRefinementMonotone2 addInEff addOutEff

testsInOutAdd (name, sample) =
    testGroup (name ++ " >+< <+>") $
        [
            testProperty "0 absorbs" (propInOutAddZero sample)
        ,
            testProperty "commutative" (propInOutAddCommutative sample)
        ,
            testProperty "associative" (propInOutAddAssociative sample)
        ,
            testProperty "refinement monotone" (propInOutAddMonotone sample)
        ]


class (RoundedAdd t, Neg t) => RoundedSubtr t where
    subtrInEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrOutEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrInEff effort a b = addInEff effort a (neg b)
    subtrOutEff effort a b = addOutEff effort a (neg b)

propInOutSubtrElim ::
    (RefOrd.PartialComparison t, RoundedSubtr t, HasZero t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> Bool
propInOutSubtrElim _ =
    roundedReflexiveCollapse zero RefOrd.pLeqEff subtrInEff subtrOutEff

propInOutSubtrNegAdd ::
    (RefOrd.PartialComparison t, RoundedSubtr t, Neg t,
     Show t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    t -> t -> Bool
propInOutSubtrNegAdd _ initEffort e1 e2 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        RefOrd.pLeqEff initEffort
    where
    expr1Up eff =
        let (>-<) = subtrInEff eff in e1 >-< (neg e2)
    expr1Dn eff =
        let (<->) = subtrOutEff eff in e1 <-> (neg e2)
    expr2Up eff =
        let (>+<) = addInEff eff in e1 >+< e2
    expr2Dn eff =
        let (<+>) = addOutEff eff in e1 <+> e2

propInOutSubtrMonotone ::
    (RefOrd.PartialComparison t, RoundedSubtr t, Show t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (AddEffortIndicator t) -> 
    (RefOrd.LEPair t) -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutSubtrMonotone _ =
    roundedRefinementMonotone2 subtrInEff subtrOutEff

testsInOutSubtr (name, sample) =
    testGroup (name ++ " >-< <->") $
        [
--            testProperty "a-a=0" (propInOutSubtrElim sample)
--            ,
            testProperty "a+b=a-(-b)" (propInOutSubtrNegAdd sample)
            ,
            testProperty "refinement monotone" (propInOutSubtrMonotone sample)
        ]


class RoundedAbs t where
    type AbsEffortIndicator t
    absDefaultEffort :: t -> AbsEffortIndicator t
    absInEff :: (AbsEffortIndicator t) -> t -> t
    absOutEff :: (AbsEffortIndicator t) -> t -> t

absOutUsingCompMax ::
    (HasZero t, Neg t, 
     NumOrd.PartialComparison t, NumOrd.OuterRoundedLattice t) =>
    (NumOrd.PartialCompareEffortIndicator t,
     NumOrd.MinmaxOuterEffortIndicator t) ->
    t -> t 
absOutUsingCompMax (effortComp, effortMinmax) a =
    case NumOrd.pCompareEff effortComp zero a of
        Just EQ -> a
        Just LT -> a
        Just LEE -> a
        Just GT -> neg a
        Just GEE -> neg a
        _ -> zero `max` (a `max` (neg a))
    where
    max = NumOrd.maxOuterEff effortMinmax

absInUsingCompMax ::
    (HasZero t, Neg t, 
     NumOrd.PartialComparison t, NumOrd.InnerRoundedLattice t) =>
    (NumOrd.PartialCompareEffortIndicator t,
     NumOrd.MinmaxInnerEffortIndicator t) ->
    t -> t 
absInUsingCompMax (effortComp, effortMinmax) a =
    case NumOrd.pCompareEff effortComp zero a of
        Just EQ -> a
        Just LT -> a
        Just LEE -> a
        Just GT -> neg a
        Just GEE -> neg a
        _ -> zero `max` (a `max` (neg a))
    where
    max = NumOrd.maxInnerEff effortMinmax

propInOutAbsNegSymmetric ::
    (RefOrd.PartialComparison t, RoundedAbs t, HasZero t, Neg t,
     Show t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    t -> Bool
propInOutAbsNegSymmetric _ =
    roundedNegSymmetric RefOrd.pLeqEff absInEff absOutEff

propInOutAbsIdempotent ::
    (RefOrd.PartialComparison t, RoundedAbs t, HasZero t,
     Show t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    t -> Bool
propInOutAbsIdempotent _ =
    roundedIdempotent RefOrd.pLeqEff absInEff absOutEff

propInOutAbsMonotone ::
    (RefOrd.PartialComparison t, RoundedAbs t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (AbsEffortIndicator t) -> 
    (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutAbsMonotone _ =
    roundedRefinementMonotone1 absInEff absOutEff

testsInOutAbs (name, sample) =
    testGroup (name ++ " in/out rounded abs") $
        [
            testProperty "neg -> no change" (propInOutAbsNegSymmetric sample)
        ,
            testProperty "idempotent" (propInOutAbsIdempotent sample)
        ,
            testProperty "refinement monotone" (propInOutAbsMonotone sample)
        ]



class RoundedMultiply t where
    type MultEffortIndicator t
    multDefaultEffort :: t -> MultEffortIndicator t
    multInEff :: MultEffortIndicator t -> t -> t -> t
    multOutEff :: MultEffortIndicator t -> t -> t -> t

propInOutMultMonotone ::
    (RefOrd.PartialComparison t, RoundedMultiply t, Show t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (MultEffortIndicator t) -> 
    (RefOrd.LEPair t) -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutMultMonotone _ =
    roundedRefinementMonotone2 multInEff multOutEff

propInOutMultOne ::
    (RefOrd.PartialComparison t, RoundedMultiply t, HasOne t,
     Show t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    t -> Bool
propInOutMultOne _ =
    roundedUnit one RefOrd.pLeqEff multInEff multOutEff

propInOutMultCommutative ::
    (RefOrd.PartialComparison t, RoundedMultiply t, HasZero t,
     Show t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    t -> t -> Bool
propInOutMultCommutative _ =
    roundedCommutative RefOrd.pLeqEff multInEff multOutEff
       
propInOutMultAssociative ::
    (RefOrd.PartialComparison t, 
     RoundedMultiply t, HasZero t,
     Show t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    t -> t -> t -> Bool
propInOutMultAssociative _ =
    roundedAssociative RefOrd.pLeqEff multInEff multOutEff

propInOutMultDistributesOverAdd ::
    (RefOrd.PartialComparison t,
     RoundedMultiply t,  RoundedAdd t,
     HasAntiConsistency t, Show t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (ConsistencyEffortIndicator t) ->
    (RefOrd.PartialCompareEffortIndicator t, 
     (MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> t -> Bool
propInOutMultDistributesOverAdd _ =
    roundedDistributive 
        RefOrd.pLeqEff 
        multInEff addInEff multOutEff addOutEff
       
    
testsInOutMult (name, sample) =
    testGroup (name ++ " >*< <*>") $
        [
            testProperty "1 absorbs" (propInOutMultOne sample)
        ,
            testProperty "commutative" (propInOutMultCommutative sample)
        ,
            testProperty "associative" (propInOutMultAssociative sample)
        ,
            testProperty "weakly distributes over +" (propInOutMultDistributesOverAdd sample)
        ,
            testProperty "refinement monotone" (propInOutMultMonotone sample)
        ]

class (RoundedAdd t, RoundedSubtr t, RoundedMultiply t, RoundedPowerToNonnegInt t) => RoundedRing t
    
class RoundedPowerToNonnegInt t where
    type PowerToNonnegIntEffortIndicator t
    powerToNonnegIntDefaultEffort :: 
        t -> PowerToNonnegIntEffortIndicator t 
    powerToNonnegIntInEff ::
        (PowerToNonnegIntEffortIndicator t) -> 
        t {-^ @x@ -} -> 
        Int {-^ @n@ (assumed >=0)-} -> 
        t {-^ @x^n@ rounded inwards -}
    powerToNonnegIntOutEff ::
        (PowerToNonnegIntEffortIndicator t) -> 
        t {-^ @x@ -} -> 
        Int {-^ @n@ (assumed >=0)-} -> 
        t {-^ @x^n@ rounded outwards -}

-- functions providing an implementation derived from rounded multiplication: 
        
type PowerToNonnegIntEffortIndicatorFromMult t =
    MultEffortIndicator t
    
powerToNonnegIntDefaultEffortFromMult a =
    multDefaultEffort a

powerToNonnegIntInEffFromMult ::
    (RoundedMultiply t, HasOne t) => 
    PowerToNonnegIntEffortIndicatorFromMult t -> 
    t -> Int -> t
powerToNonnegIntInEffFromMult effMult e n =
    powerFromMult (multInEff effMult) e n

powerToNonnegIntOutEffFromMult ::
    (RoundedMultiply t, HasOne t) => 
    PowerToNonnegIntEffortIndicatorFromMult t -> 
    t -> Int -> t
powerToNonnegIntOutEffFromMult effMult e n =
    powerFromMult (multOutEff effMult) e n

propInOutPowerMonotone ::
    (RefOrd.PartialComparison t, RoundedPowerToNonnegInt t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show t,
     Show (PowerToNonnegIntEffortIndicator t),
     EffortIndicator (PowerToNonnegIntEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    Int ->
    (PowerToNonnegIntEffortIndicator t) -> 
    (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutPowerMonotone _ nR =
    roundedRefinementMonotone1 powerNInEff powerNOutEff
    where
    n = nR `mod` 10
    powerNInEff eff x = powerToNonnegIntInEff eff x n
    powerNOutEff eff x = powerToNonnegIntOutEff eff x n


propInOutPowerSumExponents ::
    (RefOrd.PartialComparison t,
     RoundedPowerToNonnegInt t, RoundedMultiply t, 
     HasOne t, HasAntiConsistency t, Show t,
     Show (PowerToNonnegIntEffortIndicator t),
     EffortIndicator (PowerToNonnegIntEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (ConsistencyEffortIndicator t),
     EffortIndicator (ConsistencyEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (ConsistencyEffortIndicator t) -> 
    (RefOrd.PartialCompareEffortIndicator t,
     (PowerToNonnegIntEffortIndicator t,
      MultEffortIndicator t)) ->
    t -> Int -> Int -> Bool
propInOutPowerSumExponents _ effortConsistency initEffort a nR mR =
    thinEqualConsLeqRoundingUpDnImprovement [a]
        expr1Up expr1Dn expr2Up expr2Dn 
        RefOrd.pLeqEff
        effortConsistency 
        initEffort
    where
    n = nR `mod` 10
    m = mR `mod` 10
    expr1Up (effPower, effMult) =
        let (>^<) = powerToNonnegIntInEff effPower in
        let (>*<) = multInEff effMult in
        (a >^< n) >*< (a >^< m)
    expr1Dn (effPower, effMult) =
        let (<^>) = powerToNonnegIntOutEff effPower in
        let (<*>) = multOutEff effMult in
        (a <^> n) <*> (a <^> m)
    expr2Up (effPower, effMult) =
        let (>^<) = powerToNonnegIntInEff effPower in
        a >^< (n + m)
    expr2Dn (effPower, effMult) =
        let (<^>) = powerToNonnegIntOutEff effPower in
        a <^> (n + m)

testsInOutIntPower (name, sample) =
    testGroup (name ++ " non-negative integer power") $
        [
            testProperty "a^n * a^m ⊑/⊒ a^(n+m)" (propInOutPowerSumExponents sample)
            ,
            testProperty "refinement monotone" (propInOutPowerMonotone sample)
--            ,
--            testProperty "a/b=a*(1/b)" (propUpDnDivRecipMult sample)
        ]

class RoundedDivide t where
    type DivEffortIndicator t
    divDefaultEffort :: t -> DivEffortIndicator t
    divInEff :: DivEffortIndicator t -> t -> t -> t
    divOutEff :: DivEffortIndicator t -> t -> t -> t


propInOutDivMonotone ::
    (RefOrd.PartialComparison t, RoundedDivide t, Show t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DivEffortIndicator t) -> 
    (RefOrd.LEPair t) -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutDivMonotone _ =
    roundedRefinementMonotone2 divInEff divOutEff

propInOutDivElim ::
    (RefOrd.PartialComparison t, RoundedDivide t, HasOne t, HasZero t,
     Show t,
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     DivEffortIndicator t) -> 
    t -> Bool
propInOutDivElim _ efforts2@(effComp, _) a =
    let ?pCompareEffort = effComp in
    case (a ⊑? zero, zero ⊑? a) of
        (Just False, Just False) ->
            roundedReflexiveCollapse 
                one 
                RefOrd.pLeqEff 
                divInEff divOutEff 
                efforts2 
                a
        _ -> True

propInOutDivRecipMult ::
    (RefOrd.PartialComparison t,
     RoundedMultiply t, RoundedDivide t, HasOne t, HasZero t,
     Show t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     (MultEffortIndicator t, DivEffortIndicator t)) -> 
    t -> t -> Bool
propInOutDivRecipMult _ initEffort@(effComp,_) e1 e2 =
    let ?pCompareEffort = effComp in
    case (e2 ⊑? zero, zero ⊑? e2) of
        (Just False, Just False) ->
            equalRoundingUpDnBin2Var2 
                expr1 expr2 RefOrd.pLeqEff
                multInEff divInEff
                multOutEff divOutEff
                initEffort e1 e2
        _ -> True
    where
    expr1 op1Eff op2Eff (effort1, effort2) e1 e2 = 
        e1 * (one / e2)
        where
        (*) = op1Eff effort1
        (/) = op2Eff effort2
    expr2 op1Eff op2Eff (effort1, effort2) e1 e2 = 
        e1 / e2
        where
        (/) = op2Eff effort2

testsInOutDiv (name, sample) =
    testGroup (name ++ " /. /^") $
        [
--            testProperty "a/a=1" (propInOutDivElim sample)
--            ,
            testProperty "a/b=a*(1/b)" (propInOutDivRecipMult sample)
        ,
            testProperty "refinement monotone" (propInOutDivMonotone sample)
        ]

class (RoundedRing t, RoundedDivide t) => RoundedField t where
    type FieldOpsEffortIndicator t
    fieldOpsDefaultEffort :: t -> FieldOpsEffortIndicator t
    fldEffortAdd :: t -> (FieldOpsEffortIndicator t) -> (AddEffortIndicator t)
    fldEffortMult :: t ->  (FieldOpsEffortIndicator t) -> (MultEffortIndicator t)
    fldEffortPow :: t -> (FieldOpsEffortIndicator t) -> (PowerToNonnegIntEffortIndicator t)
    fldEffortDiv :: t -> (FieldOpsEffortIndicator t) -> (DivEffortIndicator t)

