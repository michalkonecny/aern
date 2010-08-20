{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent "RefinementOrderRounding.InPlace". 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.MixedFieldOps where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws 
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Control.Monad.ST
import Control.Exception
import Data.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class (RoundedMixedAdd t tn, CanBeMutable t) => RoundedMixedAddInPlace t tn where
    mixedAddInInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
    mixedAddOutInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
    mixedAddInInPlaceEff sample =
        pureToMutableNonmutEff sample mixedAddInEff
    mixedAddOutInPlaceEff sample =
        pureToMutableNonmutEff sample mixedAddOutEff

{- properties of mixed addition -}

propMixedAddInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedAddInPlace t tn, RoundedAdd t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), 
     HasInfinities (Distance t), HasZero (Distance t),
     Show (MixedAddEffortIndicator t tn),
     EffortIndicator (MixedAddEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedAddInPlaceEqualsConvert sample1 sample2 effortDistComp initEffort d n =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1In (effMAdd,_,_) =
        let (>+<|=) dR = mixedAddInInPlaceEff d effMAdd dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR >+<|= n
            unsafeReadMutable dR
    expr1Out (effMAdd,_,_) =
        let (<+>|=) dR = mixedAddOutInPlaceEff d effMAdd dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR <+>|= n
            unsafeReadMutable dR
    expr2In (_,effAdd,effConv) =
        let (>+<) = addInEff effAdd in (convertInEff effConv n) >+< d
    expr2Out (_,effAdd,effConv) =
        let (<+>) = addOutEff effAdd in (convertOutEff effConv n) <+> d



class (RoundedMixedMultiply t tn, CanBeMutable t) => RoundedMixedMultiplyInPlace t tn where
    mixedMultInInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
    mixedMultOutInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
    mixedMultInInPlaceEff sample =
        pureToMutableNonmutEff sample mixedMultInEff
    mixedMultOutInPlaceEff sample =
        pureToMutableNonmutEff sample mixedMultOutEff

{- properties of mixed multiplication -}

propMixedMultInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedMultiplyInPlace t tn, RoundedMultiply t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), 
     HasInfinities (Distance t), HasZero (Distance t),
     Show (MixedMultEffortIndicator t tn),
     EffortIndicator (MixedMultEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t,
     (MixedMultEffortIndicator t tn,      
      MultEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedMultInPlaceEqualsConvert sample1 sample2 effortDistComp initEffort d n =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1In (effMMult,_,_) =
        let (>*<|=) dR = mixedMultInInPlaceEff d effMMult dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR >*<|= n
            unsafeReadMutable dR
    expr1Out (effMMult,_,_) =
        let (<*>|=) dR = mixedMultOutInPlaceEff d effMMult dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR <*>|= n
            unsafeReadMutable dR
    expr2In (_,effMult,effConv) =
        let (>*<) = multInEff effMult in (convertInEff effConv n) >*< d
    expr2Out (_,effMult,effConv) =
        let (<*>) = multOutEff effMult in (convertOutEff effConv n) <*> d

class (RoundedMixedDivide t tn, CanBeMutable t) => RoundedMixedDivideInPlace t tn where
    mixedDivInInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
    mixedDivOutInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
    mixedDivInInPlaceEff sample =
        pureToMutableNonmutEff sample mixedDivInEff
    mixedDivOutInPlaceEff sample =
        pureToMutableNonmutEff sample mixedDivOutEff

{- properties of mixed division -}

propMixedDivInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedDivideInPlace t tn, RoundedDivide t,
     Show t, HasZero t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), 
     HasInfinities (Distance t), HasZero (Distance t),
     Show (MixedDivEffortIndicator t tn),
     EffortIndicator (MixedDivEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      DivEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedDivInPlaceEqualsConvert sample1 sample2 effortDistComp
        initEffort@(_,effComp,(_,effConv,_)) d n
    | awayFromZero =
            equalRoundingUpDnImprovement
                expr1In expr1Out expr2In expr2Out 
                RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    | otherwise = True
    where
    awayFromZero =
        case (convertOutEff effConv n, convertInEff effConv n) of
            (nOut, nIn) ->
                let ?pCompareEffort = effComp in
                case (nOut ⊑? zero, zero ⊑? nIn) of
                    (Just False, Just False) -> True
                    _ -> False && (null [d, nOut, nIn]) -- type of e2Up, e2Dn...
    expr1In (effMDiv,_,_) =
        let (>*<|=) dR = mixedDivInInPlaceEff d effMDiv dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR >*<|= n
            unsafeReadMutable dR
    expr1Out (effMDiv,_,_) =
        let (<*>|=) dR = mixedDivOutInPlaceEff d effMDiv dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR <*>|= n
            unsafeReadMutable dR
    expr2In (_,effDiv,effConv) =
        let (>*<) = divInEff effDiv in (convertInEff effConv n) >*< d
    expr2Out (_,effDiv,effConv) =
        let (<*>) = divOutEff effDiv in (convertOutEff effConv n) <*> d
    
testsInOutMixedFieldOpsInPlace (name, sample) (nameN, sampleN) =
    testGroup (name ++ " with " ++ nameN ++ ": in-place mixed up/dn rounded ops") $
        [
            testProperty "addition" (propMixedAddInPlaceEqualsConvert sample sampleN)
        ,
            testProperty "multiplication" (propMixedMultInPlaceEqualsConvert sample sampleN)
        ,
            testProperty "division" (propMixedDivInPlaceEqualsConvert sample sampleN)
        ]

class (RoundedMixedAddInPlace t tn, RoundedMixedMultiplyInPlace t tn) => RoundedMixedRingInPlace t tn

class (RoundedMixedRingInPlace t tn, RoundedMixedDivideInPlace t tn) => RoundedMixedFieldInPlace t tn
    