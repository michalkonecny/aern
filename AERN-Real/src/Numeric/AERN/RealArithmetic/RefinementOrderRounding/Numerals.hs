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

import Numeric.AERN.RealArithmetic.ExactOperations

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Data.Ratio

class FromInteger t where
    type FromIntegerEffortIndicator t
    fromIntegerInEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerOutEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDefaultEffort :: t -> FromIntegerEffortIndicator t 

propFromIntegerMonotone ::
    (FromInteger t, RefOrd.Comparison t) =>
    t ->
    (FromIntegerEffortIndicator t) ->  
    Integer -> Integer -> Bool
propFromIntegerMonotone sample effort i1 i2 
    | i1 > i2 = a1In RefOrd.|> a2Out
    | i1 <= i2 = a1Out RefOrd.|< a2In
    | otherwise = True
    where
    a1Out = fromIntegerOutEff effort i1 
    a1In = fromIntegerInEff effort i1 
    a2Out = fromIntegerOutEff effort i2 
    a2In = fromIntegerInEff effort i2 
    _ = [sample, a1Out, a1In]
    
class ToInteger t where
    type ToIntegerEffortIndicator t
    toIntegerInEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerOutEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerDefaultEffort :: t -> ToIntegerEffortIndicator t 

propToIntegerMonotone ::
    (ToInteger t, RefOrd.Comparison t) =>
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, ToIntegerEffortIndicator t) -> 
    t -> t -> Bool
propToIntegerMonotone sample (effortComp, effortConv) a1 a2 =
    case RefOrd.pCompareEff effortComp a1 a2 of
        Just LT -> i1Out < i2In
        Just LEE -> i1Out <= i2In
        Just GT -> i1In > i2Out
        Just GEE -> i1In >= i2Out
        Just EQ -> i1Out < i2In
        _ -> True
    where
    i1Out = toIntegerOutEff effortConv a1 
    i1In = toIntegerInEff effortConv a1 
    i2Out = toIntegerOutEff effortConv a2 
    i2In = toIntegerInEff effortConv a2 
    
propToFromInteger ::
    (ToInteger t, FromInteger t, RefOrd.Comparison t) =>
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     FromIntegerEffortIndicator t, 
     ToIntegerEffortIndicator t) ->
    t -> Bool
propToFromInteger _ (effortComp, effortFrom, effortTo) a =
    let ?pCompareEffort = effortComp in
    case (aOut RefOrd.|<=? a, a RefOrd.|<=? aIn) of
       (Just False, _) -> False
       (_, Just False) -> False
       _ -> True
    where
    aOut = fromIntegerOutEff effortFrom $ toIntegerOutEff effortTo a 
    aIn = fromIntegerInEff effortFrom $ toIntegerInEff effortTo a 
    
class FromDouble t where
    type FromDoubleEffortIndicator t
    fromDoubleInEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleOutEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDefaultEffort :: t -> FromDoubleEffortIndicator t 

class ToDouble t where
    type ToDoubleEffortIndicator t
    toDoubleInEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleOutEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDefaultEffort :: t -> ToDoubleEffortIndicator t 

--class FromRatio t where
--    fromRatioInEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioOutEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDefaultEffort :: t -> EffortIndicator 
--
--class ToRatio t where
--    toRatioInEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioOutEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDefaultEffort :: t -> EffortIndicator 

