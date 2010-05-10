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

import Data.Ratio

class FromInteger t where
    type FromIntegerEffortIndicator t
    fromIntegerUpEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDnEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDefaultEffort :: t -> FromIntegerEffortIndicator t 

propFromIntegerMonotone ::
    (FromInteger t, NumOrd.Comparison t) =>
    t ->
    (FromIntegerEffortIndicator t) ->  
    Integer -> Integer -> Bool
propFromIntegerMonotone sample effort i1 i2 
    | i1 > i2 = a1Up NumOrd.> a2Dn
    | i1 <= i2 = a1Dn NumOrd.< a2Up
    | otherwise = True
    where
    a1Dn = fromIntegerDnEff effort i1 
    a1Up = fromIntegerUpEff effort i1 
    a2Dn = fromIntegerDnEff effort i2 
    a2Up = fromIntegerUpEff effort i2 
    _ = [sample, a1Dn, a1Up]
    
class ToInteger t where
    type ToIntegerEffortIndicator t
    toIntegerUpEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerDnEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerDefaultEffort :: t -> ToIntegerEffortIndicator t 

propToIntegerMonotone ::
    (ToInteger t, NumOrd.Comparison t) =>
    t -> 
    (NumOrd.PartialCompareEffortIndicator t, ToIntegerEffortIndicator t) -> 
    t -> t -> Bool
propToIntegerMonotone sample (effortComp, effortConv) a1 a2 =
    case NumOrd.pCompareEff effortComp a1 a2 of
        Just LT -> i1Dn < i2Up
        Just LEE -> i1Dn <= i2Up
        Just GT -> i1Up > i2Dn
        Just GEE -> i1Up >= i2Dn
        Just EQ -> i1Dn < i2Up
        _ -> True
    where
    i1Dn = toIntegerDnEff effortConv a1 
    i1Up = toIntegerUpEff effortConv a1 
    i2Dn = toIntegerDnEff effortConv a2 
    i2Up = toIntegerUpEff effortConv a2 
    
propToFromInteger ::
    (ToInteger t, FromInteger t, NumOrd.Comparison t) =>
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
    
class FromDouble t where
    type FromDoubleEffortIndicator t
    fromDoubleUpEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDnEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDefaultEffort :: t -> FromDoubleEffortIndicator t 

class ToDouble t where
    type ToDoubleEffortIndicator t
    toDoubleUpEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDnEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDefaultEffort :: t -> ToDoubleEffortIndicator t 

--class FromRatio t where
--    fromRatioUpEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDnEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDefaultEffort :: t -> EffortIndicator 
--
--class ToRatio t where
--    toRatioUpEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDnEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDefaultEffort :: t -> EffortIndicator 

