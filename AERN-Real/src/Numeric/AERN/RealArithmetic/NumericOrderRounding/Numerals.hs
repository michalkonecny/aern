{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals
    Description :  conversion between approximations and standard numeric types  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Conversion between approximations and standard numeric types.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Misc.Maybe

import Data.Ratio

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class FromInteger t where
    type FromIntegerEffortIndicator t
    fromIntegerDefaultEffort :: t -> FromIntegerEffortIndicator t 
    fromIntegerUpEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDnEff :: FromIntegerEffortIndicator t -> Integer -> t

propFromIntegerMonotone ::
    (FromInteger t, NumOrd.PartialComparison t) =>
    t ->
    (FromIntegerEffortIndicator t, NumOrd.PartialCompareEffortIndicator t) ->  
    Integer -> Integer -> Bool
propFromIntegerMonotone sample (effortFrom, effortComp) i1 i2 
    | i1 > i2 = trueOrNothing $ let ?pCompareEffort = effortComp in a1Up NumOrd.>? a2Dn
    | i1 < i2 = trueOrNothing $ let ?pCompareEffort = effortComp in a1Dn NumOrd.<? a2Up
    | otherwise = True
    where
    a1Dn = fromIntegerDnEff effortFrom i1 
    a1Up = fromIntegerUpEff effortFrom i1 
    a2Dn = fromIntegerDnEff effortFrom i2 
    a2Up = fromIntegerUpEff effortFrom i2 
    _ = [sample, a1Dn, a1Up]
    
class ToInteger t where
    type ToIntegerEffortIndicator t
    toIntegerDefaultEffort :: t -> ToIntegerEffortIndicator t 
    toIntegerUpEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerDnEff :: ToIntegerEffortIndicator t -> t -> Integer

propToIntegerMonotone ::
    (ToInteger t, NumOrd.PartialComparison t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, ToIntegerEffortIndicator t) -> 
    t -> t -> Bool
propToIntegerMonotone sample (effortComp, effortConv) a1 a2 =
    case NumOrd.pCompareEff effortComp a1 a2 of
        Just LT -> i1Dn <= i2Up
        Just LEE -> i1Dn <= i2Up
        Just GT -> i1Up >= i2Dn
        Just GEE -> i1Up >= i2Dn
        Just EQ -> i1Dn <= i1Up
        _ -> True
    where
    i1Dn = toIntegerDnEff effortConv a1 
    i1Up = toIntegerUpEff effortConv a1 
    i2Dn = toIntegerDnEff effortConv a2 
    i2Up = toIntegerUpEff effortConv a2 
    
propToFromInteger ::
    (ToInteger t, FromInteger t, NumOrd.PartialComparison t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, 
     FromIntegerEffortIndicator t, 
     ToIntegerEffortIndicator t) ->
    t -> Bool
propToFromInteger _ (effortComp, effortFrom, effortTo) a =
    let ?pCompareEffort = effortComp in
    case (aDn NumOrd.<=? a, a NumOrd.<=? aUp) of
       (Just False, _) -> False
       (_, Just False) -> False
       _ -> True
    where
    aDn = fromIntegerDnEff effortFrom $ toIntegerDnEff effortTo a 
    aUp = fromIntegerUpEff effortFrom $ toIntegerUpEff effortTo a 
    
testsFromToInteger (name, sample) =
    testGroup (name ++ " Integer conversions") $
        [
            testProperty "from Integer monotone" (propFromIntegerMonotone sample)
        ,
            testProperty "to Integer monotone" (propToIntegerMonotone sample)
        ,
            testProperty "round trip conversion" (propToFromInteger sample)
        ]
    
class FromDouble t where
    type FromDoubleEffortIndicator t
    fromDoubleUpEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDnEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDefaultEffort :: t -> FromDoubleEffortIndicator t 

propFromDoubleMonotone ::
    (FromDouble t, NumOrd.PartialComparison t) =>
    t ->
    (FromDoubleEffortIndicator t, NumOrd.PartialCompareEffortIndicator t) ->  
    Double -> Double -> Bool
propFromDoubleMonotone sample (effortFrom, effortComp) i1 i2 
    | i1 > i2 = trueOrNothing $ let ?pCompareEffort = effortComp in a1Up NumOrd.>? a2Dn
    | i1 < i2 = trueOrNothing $ let ?pCompareEffort = effortComp in a1Dn NumOrd.<? a2Up
    | otherwise = True
    where
    a1Dn = fromDoubleDnEff effortFrom i1 
    a1Up = fromDoubleUpEff effortFrom i1 
    a2Dn = fromDoubleDnEff effortFrom i2 
    a2Up = fromDoubleUpEff effortFrom i2 
    _ = [sample, a1Dn, a1Up]

class ToDouble t where
    type ToDoubleEffortIndicator t
    toDoubleUpEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDnEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDefaultEffort :: t -> ToDoubleEffortIndicator t 

propToDoubleMonotone ::
    (ToDouble t, NumOrd.PartialComparison t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, ToDoubleEffortIndicator t) -> 
    t -> t -> Bool
propToDoubleMonotone sample (effortComp, effortConv) a1 a2 =
    case NumOrd.pCompareEff effortComp a1 a2 of
        Just LT -> i1Dn <= i2Up
        Just LEE -> i1Dn <= i2Up
        Just GT -> i1Up >= i2Dn
        Just GEE -> i1Up >= i2Dn
        Just EQ -> i1Dn <= i1Up
        _ -> True
    where
    i1Dn = toDoubleDnEff effortConv a1 
    i1Up = toDoubleUpEff effortConv a1 
    i2Dn = toDoubleDnEff effortConv a2 
    i2Up = toDoubleUpEff effortConv a2 
    
propToFromDouble ::
    (ToDouble t, FromDouble t, NumOrd.PartialComparison t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, 
     FromDoubleEffortIndicator t, 
     ToDoubleEffortIndicator t) ->
    t -> Bool
propToFromDouble _ (effortComp, effortFrom, effortTo) a =
    let ?pCompareEffort = effortComp in
    case (aDn NumOrd.<=? a, a NumOrd.<=? aUp) of
       (Just False, _) -> False
       (_, Just False) -> False
       _ -> True
    where
    aDn = fromDoubleDnEff effortFrom $ toDoubleDnEff effortTo a 
    aUp = fromDoubleUpEff effortFrom $ toDoubleUpEff effortTo a 
    
testsFromToDouble (name, sample) =
    testGroup (name ++ " Double conversions") $
        [
            testProperty "from Double monotone" (propFromDoubleMonotone sample)
        ,
            testProperty "to Double monotone" (propToDoubleMonotone sample)
        ,
            testProperty "round trip conversion" (propToFromDouble sample)
        ]
    

--class FromRatio t where
--    fromRatioUpEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDnEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDefaultEffort :: t -> EffortIndicator 
--
--class ToRatio t where
--    toRatioUpEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDnEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDefaultEffort :: t -> EffortIndicator 

