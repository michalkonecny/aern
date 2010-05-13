{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.Numerals
    Description :  conversion between approximations and standard numeric types  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Conversion between approximations and standard numeric types.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Numerals where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals as UpDnNumerals

import Numeric.AERN.Misc.Maybe

import Data.Ratio

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class FromInteger t where
    type FromIntegerEffortIndicator t
    fromIntegerInEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerOutEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDefaultEffort :: t -> FromIntegerEffortIndicator t 

propFromIntegerMonotone ::
    (FromInteger t, NumOrd.PartialComparison t) =>
    t ->
    (FromIntegerEffortIndicator t, NumOrd.PartialCompareEffortIndicator t) ->  
    Integer -> Integer -> Bool
propFromIntegerMonotone sample (effortFrom, effortComp) i1 i2 
    | i1 > i2 = 
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1Out >? a2Out)
        &&
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1In >? a2In)
    | i1 < i2 = 
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1Out <? a2Out)
        &&
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1In <? a2In)
    | otherwise = True
    where
    a1Out = fromIntegerOutEff effortFrom i1 
    a1In = fromIntegerInEff effortFrom i1 
    a2Out = fromIntegerOutEff effortFrom i2 
    a2In = fromIntegerInEff effortFrom i2 
    _ = [sample, a1Out, a1In]

propToFromInteger ::
    (UpDnNumerals.ToInteger t, FromInteger t, NumOrd.PartialComparison t, Show t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, 
     FromIntegerEffortIndicator t, 
     UpDnNumerals.ToIntegerEffortIndicator t) ->
    t -> Bool
propToFromInteger sample (effortComp, effortFrom, effortTo) a =
    let ?pCompareEffort = effortComp in
    case (aDnOut <=? a, a <=? aUpOut) of
       (Just False, _) -> printErrorDetail
       (_, Just False) -> printErrorDetail
       _ -> True
    where
    aDnOut = fromIntegerOutEff effortFrom $ UpDnNumerals.toIntegerDnEff effortTo a 
    aUpOut = fromIntegerOutEff effortFrom $ UpDnNumerals.toIntegerUpEff effortTo a 
    _ = [sample, aDnOut]
    printErrorDetail =
        error $
           "propToFromInteger failed:"
           ++ "\n  a = " ++ show a
           ++ "\n  aDnOut = " ++ show aDnOut
           ++ "\n  aUpOut = " ++ show aUpOut


testsFromToInteger (name, sample) =
    testGroup (name ++ " Integer conversions") $
        [
            testProperty "from Integer monotone" (propFromIntegerMonotone sample)
        ,
            testProperty "to Integer monotone" (UpDnNumerals.propToIntegerMonotone sample)
        ,
            testProperty "round trip conversion" (propToFromInteger sample)
        ]

class FromDouble t where
    type FromDoubleEffortIndicator t
    fromDoubleInEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleOutEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDefaultEffort :: t -> FromDoubleEffortIndicator t 

propFromDoubleMonotone ::
    (FromDouble t, NumOrd.PartialComparison t) =>
    t ->
    (FromDoubleEffortIndicator t, NumOrd.PartialCompareEffortIndicator t) ->  
    Double -> Double -> Bool
propFromDoubleMonotone sample (effortFrom, effortComp) i1 i2 
    | i1 > i2 = 
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1Out >? a2Out)
        &&
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1In >? a2In)
    | i1 < i2 = 
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1Out <? a2Out)
        &&
        (trueOrNothing $ let ?pCompareEffort = effortComp in a1In <? a2In)
    | otherwise = True
    where
    a1Out = fromDoubleOutEff effortFrom i1 
    a1In = fromDoubleInEff effortFrom i1 
    a2Out = fromDoubleOutEff effortFrom i2 
    a2In = fromDoubleInEff effortFrom i2 
    _ = [sample, a1Out, a1In]

propToFromDouble ::
    (UpDnNumerals.ToDouble t, FromDouble t, NumOrd.PartialComparison t, Show t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, 
     FromDoubleEffortIndicator t, 
     UpDnNumerals.ToDoubleEffortIndicator t) ->
    t -> Bool
propToFromDouble sample (effortComp, effortFrom, effortTo) a =
    let ?pCompareEffort = effortComp in
    case (aDnOut <=? a, a <=? aUpOut) of
       (Just False, _) -> printErrorDetail
       (_, Just False) -> printErrorDetail
       _ -> True
    where
    aDnOut = fromDoubleOutEff effortFrom $ UpDnNumerals.toDoubleDnEff effortTo a 
    aUpOut = fromDoubleOutEff effortFrom $ UpDnNumerals.toDoubleUpEff effortTo a 
    _ = [sample, aDnOut]
    printErrorDetail =
        error $
           "propToFromDouble failed:"
           ++ "\n  a = " ++ show a
           ++ "\n  aDnOut = " ++ show aDnOut
           ++ "\n  aUpOut = " ++ show aUpOut


testsFromToDouble (name, sample) =
    testGroup (name ++ " Double conversions") $
        [
            testProperty "from Double monotone" (propFromDoubleMonotone sample)
        ,
            testProperty "to Double monotone" (UpDnNumerals.propToDoubleMonotone sample)
        ,
            testProperty "round trip conversion" (propToFromDouble sample)
        ]

--class FromRatio t where
--    fromRatioInEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioOutEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDefaultEffort :: t -> EffortIndicator 
