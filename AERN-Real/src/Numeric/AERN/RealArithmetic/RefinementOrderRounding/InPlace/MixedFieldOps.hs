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

class (RoundedMixedAddEffort t tn, CanBeMutable t) => 
    RoundedMixedAddInPlace t tn 
    where
    mixedAddInInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
    mixedAddOutInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s

mixedAddInInPlaceEffFromPure sample =
    pureToMutableNonmutEff sample mixedAddInEff
mixedAddOutInPlaceEffFromPure sample =
    pureToMutableNonmutEff sample mixedAddOutEff

mixedAddInInPlaceEffFromInPlace sample = 
    pureToMutableNonmutEff $ mixedAddInInPlaceEff sample 
mixedAddOutInPlaceEffFromInPlace sample = 
    pureToMutableNonmutEff $ mixedAddOutInPlaceEff sample 

{- properties of mixed addition -}

propMixedAddInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedAddInPlace t tn, 
     RoundedMixedAdd t tn, 
     RoundedAdd t,
     Show t,
     Show (MixedAddEffortIndicator t tn),
     EffortIndicator (MixedAddEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedAddInPlaceEqualsConvert sample1 sample2 initEffort d n =
    equalRoundingUpDn
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
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



class (RoundedMixedMultiplyEffort t tn, CanBeMutable t) => 
    RoundedMixedMultiplyInPlace t tn 
    where
    mixedMultInInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
    mixedMultOutInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s

mixedMultInInPlaceEffFromPure sample =
    pureToMutableNonmutEff sample mixedMultInEff
mixedMultOutInPlaceEffFromPure sample =
    pureToMutableNonmutEff sample mixedMultOutEff

mixedMultInInPlaceEffFromInPlace sample = 
    pureToMutableNonmutEff $ mixedMultInInPlaceEff sample 
mixedMultOutInPlaceEffFromInPlace sample = 
    pureToMutableNonmutEff $ mixedMultOutInPlaceEff sample

{- properties of mixed multiplication -}

propMixedMultInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedMultiplyInPlace t tn, 
     RoundedMixedMultiply t tn, 
     RoundedMultiply t,
     Show t,
     Show (MixedMultEffortIndicator t tn),
     EffortIndicator (MixedMultEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedMultEffortIndicator t tn,      
      MultEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedMultInPlaceEqualsConvert sample1 sample2 initEffort d n =
    equalRoundingUpDn
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
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

class (RoundedMixedDivideEffort t tn, CanBeMutable t) => 
    RoundedMixedDivideInPlace t tn 
    where
    mixedDivInInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
    mixedDivOutInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s

mixedDivInInPlaceEffFromPure sample =
    pureToMutableNonmutEff sample mixedDivInEff
mixedDivOutInPlaceEffFromPure sample =
    pureToMutableNonmutEff sample mixedDivOutEff

mixedDivInInPlaceEffFromInPlace sample = 
    pureToMutableNonmutEff $ mixedDivInInPlaceEff sample 
mixedDivOutInPlaceEffFromInPlace sample = 
    pureToMutableNonmutEff $ mixedDivOutInPlaceEff sample

{- properties of mixed division -}

propMixedDivInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedDivideInPlace t tn, 
     RoundedMixedDivide t tn, 
     RoundedDivide t,
     Show t, HasZero t,
     Show (MixedDivEffortIndicator t tn),
     EffortIndicator (MixedDivEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      DivEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedDivInPlaceEqualsConvert sample1 sample2
        initEffort@(effComp,(_,effConv,_)) d n
    | awayFromZero =
            equalRoundingUpDn
                expr1In expr1Out expr2In expr2Out 
                RefOrd.pLeqEff initEffort
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
    