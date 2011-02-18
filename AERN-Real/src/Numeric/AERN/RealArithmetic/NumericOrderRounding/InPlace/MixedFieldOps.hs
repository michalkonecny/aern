{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent "NumericOrderRounding.InPlace". 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.MixedFieldOps where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.FieldOps

import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws 
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import Control.Monad.ST
import Control.Exception
import Data.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class (RoundedMixedAdd t tn, CanBeMutable t) => RoundedMixedAddInPlace t tn where
    mixedAddUpInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
    mixedAddDnInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedAddEffortIndicator t tn) t tn s
    mixedAddUpInPlaceEff sample =
        pureToMutableNonmutEff sample mixedAddUpEff
    mixedAddDnInPlaceEff sample =
        pureToMutableNonmutEff sample mixedAddDnEff

-- an alternative default implementation using conversion 
-- - this could be more efficient

mixedAddUpInPlaceEffByConversion ::
    (Convertible tn t, RoundedAddInPlace t, Show tn) =>
    t ->
    OpMutableNonmutEff (AddEffortIndicator t, ConvertEffortIndicator tn t) t tn s 
mixedAddUpInPlaceEffByConversion sample (effAdd, effConv) rM dM n =
    do
    nUpM <- makeMutable nUp
    addUpInPlaceEff sample effAdd rM dM nUpM
    where
    _ = [nUp, sample]
    nUp = 
        case convertUpEff effConv n of
            (Just nUp) -> nUp
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed addition: n = " ++ show n

mixedAddDnInPlaceEffByConversion ::
    (Convertible tn t, RoundedAddInPlace t, Show tn) =>
    t ->
    OpMutableNonmutEff (AddEffortIndicator t, ConvertEffortIndicator tn t) t tn s 
mixedAddDnInPlaceEffByConversion sample (effAdd, effConv) rM dM n =
    do
    nDnM <- makeMutable nDn
    addDnInPlaceEff sample effAdd rM dM nDnM
    where
    _ = [nDn, sample]
    nDn = 
        case convertDnEff effConv n of
            (Just nDn) -> nDn
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed addition: n = " ++ show n


{- properties of mixed addition -}

propMixedAddInPlaceEqualsConvert ::
    (NumOrd.PartialComparison t, Convertible tn t,
     RoundedMixedAddInPlace t tn, RoundedAdd t,
     Show t,
     Show (MixedAddEffortIndicator t tn),
     EffortIndicator (MixedAddEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedAddInPlaceEqualsConvert sample1 sample2 initEffort d n =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    expr1Up (effMAdd,_,_) =
        let (+^|=) dR = mixedAddUpInPlaceEff d effMAdd dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR +^|= n
            unsafeReadMutable dR
    expr1Dn (effMAdd,_,_) =
        let (+.|=) dR = mixedAddDnInPlaceEff d effMAdd dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR +.|= n
            unsafeReadMutable dR
    expr2Up (_,effAdd,effConv) =
        let (+^) = addUpEff effAdd in (fromJust $ convertUpEff effConv n) +^ d
    expr2Dn (_,effAdd,effConv) =
        let (+.) = addDnEff effAdd in (fromJust $ convertDnEff effConv n) +. d



class (RoundedMixedMultiply t tn, CanBeMutable t) => RoundedMixedMultiplyInPlace t tn where
    mixedMultUpInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
    mixedMultDnInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedMultEffortIndicator t tn) t tn s
    mixedMultUpInPlaceEff sample =
        pureToMutableNonmutEff sample mixedMultUpEff
    mixedMultDnInPlaceEff sample =
        pureToMutableNonmutEff sample mixedMultDnEff

{- properties of mixed multiplication -}

propMixedMultInPlaceEqualsConvert ::
    (NumOrd.PartialComparison t,  NumOrd.RoundedLattice t,
     Convertible tn t,
     RoundedMixedMultiplyInPlace t tn, RoundedMultiply t,
     Show t,
     Show (MixedMultEffortIndicator t tn),
     EffortIndicator (MixedMultEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.MinmaxEffortIndicator t),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator t,
     (MixedMultEffortIndicator t tn,      
      (MultEffortIndicator t,
       ConvertEffortIndicator tn t,
       NumOrd.MinmaxEffortIndicator t))) -> 
    t -> tn -> Bool
propMixedMultInPlaceEqualsConvert sample1 sample2 initEffort d n =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    expr1Up (effMMult,_) =
        let (*^|=) dR = mixedMultUpInPlaceEff d effMMult dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR *^|= n
            unsafeReadMutable dR
    expr1Dn (effMMult,_) =
        let (*.|=) dR = mixedMultDnInPlaceEff d effMMult dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR *.|= n
            unsafeReadMutable dR
    expr2Up (_,(effMult,effConv,effMinmax)) =
        let (*^) = multUpEff effMult in
        NumOrd.maxUpEff effMinmax  
            (d *^ (fromJust $ convertUpEff effConv n))
            (d *^ (fromJust $ convertDnEff effConv n))
    expr2Dn (_,(effMult,effConv,effMinmax)) =
        let (*.) = multDnEff effMult in
        NumOrd.minDnEff effMinmax  
            (d *. (fromJust $ convertUpEff effConv n))
            (d *. (fromJust $ convertDnEff effConv n))

class (RoundedMixedDivide t tn, CanBeMutable t) => RoundedMixedDivideInPlace t tn where
    mixedDivUpInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
    mixedDivDnInPlaceEff :: 
        t -> OpMutableNonmutEff (MixedDivEffortIndicator t tn) t tn s
    mixedDivUpInPlaceEff sample =
        pureToMutableNonmutEff sample mixedDivUpEff
    mixedDivDnInPlaceEff sample =
        pureToMutableNonmutEff sample mixedDivDnEff

{- properties of mixed division -}

propMixedDivInPlaceEqualsConvert ::
    (NumOrd.PartialComparison t,  NumOrd.RoundedLattice t,
     Convertible tn t,
     RoundedMixedDivideInPlace t tn, RoundedDivide t,
     Show t, HasZero t,
     Show (MixedDivEffortIndicator t tn),
     EffortIndicator (MixedDivEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (NumOrd.MinmaxEffortIndicator t),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      (DivEffortIndicator t,
       ConvertEffortIndicator tn t,
       NumOrd.MinmaxEffortIndicator t))) -> 
    t -> tn -> Bool
propMixedDivInPlaceEqualsConvert sample1 sample2 
        initEffort@(effComp,(_,(_,effConv,_))) d n
    | awayFromZero =
            equalRoundingUpDn
                expr1Up expr1Dn expr2Up expr2Dn 
                NumOrd.pLeqEff initEffort
    | otherwise = True
    where
    awayFromZero =
        case (convertDnEff effConv n, convertUpEff effConv n) of
            (Just nDn, Just nUp) ->
                let ?pCompareEffort = effComp in
                case (nUp <? zero, zero <? nDn) of
                    (Just True, _) -> True
                    (_, Just True) -> True
                    _ -> False && (null [d, nUp, nDn]) -- type of nUp, nDn...
    expr1Up (effMDiv,_) =
        let (/^|=) dR = mixedDivUpInPlaceEff d effMDiv dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR /^|= n
            unsafeReadMutable dR
    expr1Dn (effMDiv,_) =
        let (/.|=) dR = mixedDivDnInPlaceEff d effMDiv dR dR in
        runST $ 
            do
            dR <- makeMutable d
            dR /.|= n
            unsafeReadMutable dR
    expr2Up (_,(effDiv,effConv,effMinmax)) =
        let (/^) = divUpEff effDiv in
        NumOrd.maxUpEff effMinmax  
            (d /^ (fromJust $ convertUpEff effConv n))
            (d /^ (fromJust $ convertDnEff effConv n))
    expr2Dn (_,(effDiv,effConv,effMinmax)) =
        let (/.) = divDnEff effDiv in
        NumOrd.minDnEff effMinmax  
            (d /. (fromJust $ convertUpEff effConv n))
            (d /. (fromJust $ convertDnEff effConv n))
    
testsUpDnMixedFieldOpsInPlace (name, sample) (nameN, sampleN) =
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
    