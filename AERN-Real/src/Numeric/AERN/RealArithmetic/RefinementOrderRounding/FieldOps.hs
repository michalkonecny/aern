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
    RoundedAdd(..), RoundedAddEffort(..), RoundedSubtr(..), 
    testsInOutAdd, testsInOutSubtr,
    RoundedAbs(..), RoundedAbsEffort(..), 
    testsInOutAbs,  absInUsingCompMax, absOutUsingCompMax,
    RoundedMultiply(..), RoundedMultiplyEffort(..), testsInOutMult,
    RoundedPowerToNonnegInt(..), RoundedPowerToNonnegIntEffort(..), 
    testsInOutIntPower,
    PowerToNonnegIntEffortIndicatorFromMult, powerToNonnegIntDefaultEffortFromMult,
    powerToNonnegIntInEffFromMult, powerToNonnegIntOutEffFromMult,
    RoundedDivide(..), RoundedDivideEffort(..), testsInOutDiv,
    RoundedRingEffort(..), RoundedFieldEffort(..),
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
import Numeric.AERN.Basics.Exception (HasLegalValues)
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

class RoundedAddEffort t where
    type AddEffortIndicator t
    addDefaultEffort :: t -> AddEffortIndicator t

class (RoundedAddEffort t) => RoundedAdd t where
    addInEff :: AddEffortIndicator t -> t -> t -> t
    addOutEff :: AddEffortIndicator t -> t -> t -> t

--propAddMonotone _ effortDist

propInOutAddZero ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Bool
propInOutAddZero _ effort (RefOrd.UniformlyOrderedSingleton e) =
    roundedUnit zero RefOrd.pLeqEff addInEff addOutEff effort e

propInOutAddCommutative ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> 
    Bool
propInOutAddCommutative _ effort (RefOrd.UniformlyOrderedPair (e1,e2)) =
    roundedCommutative RefOrd.pLeqEff addInEff addOutEff effort e1 e2

propInOutAddAssociative ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedTriple t) -> 
    Bool
propInOutAddAssociative _ effort (RefOrd.UniformlyOrderedTriple (e1,e2,e3)) =
    roundedAssociative RefOrd.pLeqEff addInEff addOutEff effort e1 e2 e3

propInOutAddMonotone ::
    (RefOrd.PartialComparison t, RoundedAdd t, 
     Show t, HasLegalValues t,
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
    roundedRefinementMonotone2 "addition" addInEff addOutEff

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
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Bool
propInOutSubtrElim _ effort (RefOrd.UniformlyOrderedSingleton e) =
    roundedReflexiveCollapse zero RefOrd.pLeqEff subtrInEff subtrOutEff effort e

propInOutSubtrNegAdd ::
    (RefOrd.PartialComparison t, RoundedSubtr t, Neg t,
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> 
    Bool
propInOutSubtrNegAdd _ initEffort (RefOrd.UniformlyOrderedPair (e1, e2)) =
    equalRoundingUpDn "a+b=a-(-b)"
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
    (RefOrd.PartialComparison t, RoundedSubtr t, 
     Show t, HasLegalValues t,
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
    roundedRefinementMonotone2 "subtraction" subtrInEff subtrOutEff

testsInOutSubtr (name, sample) =
    testGroup (name ++ " >-< <->") $
        [
--            testProperty "a-a=0" (propInOutSubtrElim sample)
--            ,
            testProperty "a+b=a-(-b)" (propInOutSubtrNegAdd sample)
            ,
            testProperty "refinement monotone" (propInOutSubtrMonotone sample)
        ]


class RoundedAbsEffort t where
    type AbsEffortIndicator t
    absDefaultEffort :: t -> AbsEffortIndicator t

class (RoundedAbsEffort t) => RoundedAbs t where
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
    max = NumOrd.maxOutEff effortMinmax

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
    max = NumOrd.maxInEff effortMinmax

propInOutAbsNegSymmetric ::
    (RefOrd.PartialComparison t, RoundedAbs t, HasZero t, Neg t,
     Show t, HasLegalValues t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Bool
propInOutAbsNegSymmetric _ effort (RefOrd.UniformlyOrderedSingleton e) =
    roundedNegSymmetric RefOrd.pLeqEff absInEff absOutEff effort e

propInOutAbsIdempotent ::
    (RefOrd.PartialComparison t, RoundedAbs t, HasZero t,
     Show t, HasLegalValues t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Bool
propInOutAbsIdempotent _ effort (RefOrd.UniformlyOrderedSingleton e) =
    roundedIdempotent RefOrd.pLeqEff absInEff absOutEff effort e

propInOutAbsMonotone ::
    (RefOrd.PartialComparison t, RoundedAbs t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show t, HasLegalValues t,
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
    roundedRefinementMonotone1 "abs" absInEff absOutEff

testsInOutAbs (name, sample) =
    testGroup (name ++ " in/out rounded abs") $
        [
            testProperty "neg -> no change" (propInOutAbsNegSymmetric sample)
        ,
            testProperty "idempotent" (propInOutAbsIdempotent sample)
        ,
            testProperty "refinement monotone" (propInOutAbsMonotone sample)
        ]


class RoundedMultiplyEffort t where
    type MultEffortIndicator t
    multDefaultEffort :: t -> MultEffortIndicator t

class (RoundedMultiplyEffort t) => RoundedMultiply t where
    multInEff :: MultEffortIndicator t -> t -> t -> t
    multOutEff :: MultEffortIndicator t -> t -> t -> t

propInOutMultMonotone ::
    (RefOrd.PartialComparison t, RoundedMultiply t, 
     Show t, HasLegalValues t,
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
    roundedRefinementMonotone2 "multiplication" multInEff multOutEff

propInOutMultOne ::
    (RefOrd.PartialComparison t, RoundedMultiply t, HasOne t,
     Show t, HasLegalValues t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Bool
propInOutMultOne _ effort (RefOrd.UniformlyOrderedSingleton e) =
    roundedUnit one RefOrd.pLeqEff multInEff multOutEff effort e

propInOutMultCommutative ::
    (RefOrd.PartialComparison t, RoundedMultiply t, HasZero t,
     Show t, HasLegalValues t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> 
    Bool
propInOutMultCommutative _ effort (RefOrd.UniformlyOrderedPair (e1,e2)) =
    roundedCommutative RefOrd.pLeqEff multInEff multOutEff effort e1 e2
       
propInOutMultAssociative ::
    (RefOrd.PartialComparison t, 
     RoundedMultiply t, HasZero t,
     Show t, HasLegalValues t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedTriple t) -> 
    Bool
propInOutMultAssociative _ effort (RefOrd.UniformlyOrderedTriple (e1,e2,e3)) =
    roundedAssociative RefOrd.pLeqEff multInEff multOutEff effort e1 e2 e3

propInOutMultDistributesOverAdd ::
    (RefOrd.PartialComparison t,
     RoundedMultiply t,  RoundedAdd t,
     HasAntiConsistency t, Show t, HasLegalValues t,
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
    (RefOrd.UniformlyOrderedTriple t) -> 
    Bool
propInOutMultDistributesOverAdd _ effortConst effort (RefOrd.UniformlyOrderedTriple (e1,e2,e3)) =
    roundedDistributive 
        RefOrd.pLeqEff 
        multInEff addInEff multOutEff addOutEff
        effortConst effort e1 e2 e3
       
    
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

class RoundedPowerToNonnegIntEffort t where
    type PowerToNonnegIntEffortIndicator t
    powerToNonnegIntDefaultEffort :: 
        t -> PowerToNonnegIntEffortIndicator t 

class (RoundedPowerToNonnegIntEffort t) => RoundedPowerToNonnegInt t where
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
     Show t, HasLegalValues t,
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
    roundedRefinementMonotone1 "non-neg integer power" powerNInEff powerNOutEff
    where
    n = nR `mod` 10
    powerNInEff eff x = powerToNonnegIntInEff eff x n
    powerNOutEff eff x = powerToNonnegIntOutEff eff x n


propInOutPowerSumExponents ::
    (RefOrd.PartialComparison t,
     RoundedPowerToNonnegInt t, RoundedMultiply t, 
     HasOne t, HasAntiConsistency t, Show t, HasLegalValues t,
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
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Int -> Int -> Bool
propInOutPowerSumExponents _ effortConsistency initEffort 
        (RefOrd.UniformlyOrderedSingleton a) nR mR =
    thinEqualConsLeqRoundingUpDnImprovement "a^n * a^m ⊑/⊒ a^(n+m)" [a]
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

class RoundedDivideEffort t where
    type DivEffortIndicator t
    divDefaultEffort :: t -> DivEffortIndicator t

class (HasOne t, RoundedDivideEffort t) => RoundedDivide t where
    divInEff :: DivEffortIndicator t -> t -> t -> t
    divOutEff :: DivEffortIndicator t -> t -> t -> t
    recipInEff :: DivEffortIndicator t -> t -> t
    recipOutEff :: DivEffortIndicator t -> t -> t
    recipInEff eff = divInEff eff one
    recipOutEff eff = divOutEff eff one


propInOutDivMonotone ::
    (RefOrd.PartialComparison t, RoundedDivide t, 
     Show t, HasLegalValues t,
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
    roundedRefinementMonotone2 "division" divInEff divOutEff

propInOutDivElim ::
    (RefOrd.PartialComparison t, RoundedDivide t, HasOne t, HasZero t,
     Show t, HasLegalValues t,
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     DivEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    Bool
propInOutDivElim _ efforts2@(effComp, _) (RefOrd.UniformlyOrderedSingleton a) =
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
     Show t, HasLegalValues t,
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
    (RefOrd.UniformlyOrderedPair t) -> 
    Bool
propInOutDivRecipMult _ initEffort@(effComp,_) (RefOrd.UniformlyOrderedPair (e1, e2)) =
    let ?pCompareEffort = effComp in
    case (e2 ⊑? zero, zero ⊑? e2) of
        (Just False, Just False) ->
            equalRoundingUpDnBin2Var2 "a/b=a*(1/b)"
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
    testGroup (name ++ " </> >/<") $
        [
--            testProperty "a/a=1" (propInOutDivElim sample)
--            ,
            testProperty "a/b=a*(1/b)" (propInOutDivRecipMult sample)
        ,
            testProperty "refinement monotone" (propInOutDivMonotone sample)
        ]

class 
        (RoundedAddEffort t, 
         RoundedMultiplyEffort t, 
         RoundedPowerToNonnegIntEffort t) => 
    RoundedRingEffort t
    where
    type RingOpsEffortIndicator t
    ringOpsDefaultEffort :: t -> RingOpsEffortIndicator t
    ringEffortAdd :: t -> (RingOpsEffortIndicator t) -> (AddEffortIndicator t)
    ringEffortMult :: t ->  (RingOpsEffortIndicator t) -> (MultEffortIndicator t)
    ringEffortPow :: t -> (RingOpsEffortIndicator t) -> (PowerToNonnegIntEffortIndicator t)

class 
        (RoundedAdd t, 
         RoundedSubtr t, 
         RoundedMultiply t, 
         RoundedPowerToNonnegInt t,
         RoundedRingEffort t) => 
    RoundedRing t
    
class (RoundedRingEffort t, RoundedDivideEffort t) => RoundedFieldEffort t where
    type FieldOpsEffortIndicator t
    fieldOpsDefaultEffort :: t -> FieldOpsEffortIndicator t
    fldEffortAdd :: t -> (FieldOpsEffortIndicator t) -> (AddEffortIndicator t)
    fldEffortMult :: t ->  (FieldOpsEffortIndicator t) -> (MultEffortIndicator t)
    fldEffortPow :: t -> (FieldOpsEffortIndicator t) -> (PowerToNonnegIntEffortIndicator t)
    fldEffortDiv :: t -> (FieldOpsEffortIndicator t) -> (DivEffortIndicator t)

class (RoundedRing t, RoundedDivide t, RoundedFieldEffort t) => RoundedField t
