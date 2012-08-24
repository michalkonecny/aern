{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonoLocalBinds #-}

{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent "RefinementOrderRounding.InPlace". 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.MixedFieldOps where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws 
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

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
        OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
    mixedAddOutInPlaceEff :: 
        OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s

mixedAddInInPlaceEffFromPure,
 mixedAddOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedMixedAdd t tn) =>
    OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
mixedAddInInPlaceEffFromPure =
    pureToMutableNonmutEff mixedAddInEff
mixedAddOutInPlaceEffFromPure =
    pureToMutableNonmutEff mixedAddOutEff

mixedAddInInPlaceEffFromInPlace
 ,mixedAddOutInPlaceEffFromInPlace ::
    (RoundedMixedAddInPlace t tn) =>
    (MixedAddEffortIndicator t tn) -> t -> tn -> t 
mixedAddInInPlaceEffFromInPlace = 
    mutableNonmutEffToPure mixedAddInInPlaceEff
mixedAddOutInPlaceEffFromInPlace = 
    mutableNonmutEffToPure mixedAddOutInPlaceEff 

{- properties of mixed addition -}

propMixedAddInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedAddInPlace t tn, 
     RoundedMixedAdd t tn, 
     RoundedAdd t,
     Show t, HasLegalValues t) 
    =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    tn -> Bool
propMixedAddInPlaceEqualsConvert sample _sampleN initEffort 
        (RefOrd.UniformlyOrderedSingleton d) n =
    equalRoundingUpDn "mixed in-place addition"
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
    where
    expr1In (effMAdd,_,_) =
        let (>+<|=) dR = mixedAddInInPlaceEff effMAdd dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR >+<|= n
            unsafeReadMutable dR
    expr1Out (effMAdd,_,_) =
        let (<+>|=) dR = mixedAddOutInPlaceEff effMAdd dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR <+>|= n
            unsafeReadMutable dR
    expr2In (_,effAdd,effConv) =
        let (>+<) = addInEff effAdd in (convertInEff effConv sample n) >+< d
    expr2Out (_,effAdd,effConv) =
        let (<+>) = addOutEff effAdd in (convertOutEff effConv sample n) <+> d



class (RoundedMixedMultiplyEffort t tn, CanBeMutable t) => 
    RoundedMixedMultiplyInPlace t tn 
    where
    mixedMultInInPlaceEff :: 
        OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
    mixedMultOutInPlaceEff :: 
        OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s

mixedMultInInPlaceEffFromPure,
 mixedMultOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedMixedMultiply t tn) =>
    OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
mixedMultInInPlaceEffFromPure =
    pureToMutableNonmutEff mixedMultInEff
mixedMultOutInPlaceEffFromPure =
    pureToMutableNonmutEff mixedMultOutEff

mixedMultInInPlaceEffFromInPlace,
 mixedMultOutInPlaceEffFromInPlace ::
    (RoundedMixedMultiplyInPlace t tn) =>
    (MixedMultEffortIndicator t tn) -> t -> tn -> t
mixedMultInInPlaceEffFromInPlace = 
    mutableNonmutEffToPure mixedMultInInPlaceEff 
mixedMultOutInPlaceEffFromInPlace = 
    mutableNonmutEffToPure mixedMultOutInPlaceEff

{- properties of mixed multiplication -}

propMixedMultInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedMultiplyInPlace t tn, 
     RoundedMixedMultiply t tn, 
     RoundedMultiply t,
     Show t, HasLegalValues t) 
    =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedMultEffortIndicator t tn,      
      MultEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    tn -> Bool
propMixedMultInPlaceEqualsConvert sample _sampleN initEffort 
        (RefOrd.UniformlyOrderedSingleton d) n =
    equalRoundingUpDn "in-place mixed multiplication"
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
    where
    expr1In (effMMult,_,_) =
        let (>*<|=) dR = mixedMultInInPlaceEff effMMult dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR >*<|= n
            unsafeReadMutable dR
    expr1Out (effMMult,_,_) =
        let (<*>|=) dR = mixedMultOutInPlaceEff effMMult dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR <*>|= n
            unsafeReadMutable dR
    expr2In (_,effMult,effConv) =
        let (>*<) = multInEff effMult in (convertInEff effConv sample n) >*< d
    expr2Out (_,effMult,effConv) =
        let (<*>) = multOutEff effMult in (convertOutEff effConv sample n) <*> d

class (RoundedMixedDivideEffort t tn, CanBeMutable t) => 
    RoundedMixedDivideInPlace t tn 
    where
    mixedDivInInPlaceEff :: 
        OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
    mixedDivOutInPlaceEff :: 
        OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s

mixedDivInInPlaceEffFromPure,
 mixedDivOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedMixedDivide t tn) =>
    OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
mixedDivInInPlaceEffFromPure =
    pureToMutableNonmutEff mixedDivInEff
mixedDivOutInPlaceEffFromPure =
    pureToMutableNonmutEff mixedDivOutEff

mixedDivInInPlaceEffFromInPlace,
 mixedDivOutInPlaceEffFromInPlace ::
    (RoundedMixedDivideInPlace t tn) =>
    (MixedDivEffortIndicator t tn) -> t -> tn -> t 
mixedDivInInPlaceEffFromInPlace = 
    mutableNonmutEffToPure mixedDivInInPlaceEff 
mixedDivOutInPlaceEffFromInPlace = 
    mutableNonmutEffToPure mixedDivOutInPlaceEff

mixedDivInInPlaceEffByConversion ::
    (Convertible tn t, RoundedDivideInPlace t) =>
    (DivEffortIndicator t, ConvertEffortIndicator tn t) ->
    OpMutableNonmut t tn s
mixedDivInInPlaceEffByConversion (effDiv, effConv) rM dM n =
    do
    sample <- readMutable dM
    let nConverted = convertInEff effConv sample n
    nM <- unsafeMakeMutable nConverted
    divInInPlaceEff effDiv rM dM nM

mixedDivOutInPlaceEffByConversion ::
    (Convertible tn t, RoundedDivideInPlace t) =>
    (DivEffortIndicator t, ConvertEffortIndicator tn t) ->
    OpMutableNonmut t tn s
mixedDivOutInPlaceEffByConversion (effDiv, effConv) rM dM n =
    do
    sample <- readMutable dM
    let nConverted = convertOutEff effConv sample n
    nM <- unsafeMakeMutable nConverted
    divOutInPlaceEff effDiv rM dM nM

{- properties of mixed division -}

propMixedDivInPlaceEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedDivideInPlace t tn, 
     RoundedMixedDivide t tn, 
     RoundedDivide t,
     Show t, HasZero t, HasLegalValues t) 
    =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      DivEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    tn -> Bool
propMixedDivInPlaceEqualsConvert sample _sampleN
        initEffort@(effComp,(_,effConv,_)) 
        (RefOrd.UniformlyOrderedSingleton d) n
    =
    equalRoundingUpDn "in-place mixed division"
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
    where
    expr1In (effMDiv,_,_) =
        let (>/<|=) dR = mixedDivInInPlaceEff effMDiv dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR >/<|= n
            unsafeReadMutable dR
    expr1Out (effMDiv,_,_) =
        let (</>|=) dR = mixedDivOutInPlaceEff effMDiv dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR </>|= n
            unsafeReadMutable dR
    expr2In (_,effDiv,effConv) =
        let (>/<) = divInEff effDiv in d >/< (convertInEff effConv sample n)
    expr2Out (_,effDiv,effConv) =
        let (</>) = divOutEff effDiv in d </> (convertOutEff effConv sample n)
    
testsInOutMixedFieldOpsInPlace (name, sample) (nameN, sampleN) =
    testGroup (name ++ " with " ++ nameN ++ ": in-place mixed up/dn rounded ops") $
        [
            testProperty "addition" (propMixedAddInPlaceEqualsConvert sample sampleN)
        ,
            testProperty "multiplication" (propMixedMultInPlaceEqualsConvert sample sampleN)
        ,
            testProperty "division" (propMixedDivInPlaceEqualsConvert sample sampleN)
        ]

class 
    (RoundedMixedAddInPlace t tn, 
     RoundedMixedMultiplyInPlace t tn,
     RoundedMixedRingEffort t tn)
    => 
    RoundedMixedRingInPlace t tn

class 
    (RoundedMixedRingInPlace t tn, 
     RoundedMixedDivideInPlace t tn, 
     RoundedMixedFieldEffort t tn) 
    => 
    RoundedMixedFieldInPlace t tn
    