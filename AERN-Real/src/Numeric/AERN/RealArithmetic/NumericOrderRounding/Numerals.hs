{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.NumericOrderRounding.Numerals
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

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Data.Ratio

class HasZero t where
    zero :: t
    
class HasOne t where
    one :: t
    
class FromInteger t where
    type FromIntegerEffortIndicator t
    fromIntegerUpEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDnEff :: FromIntegerEffortIndicator t -> Integer -> t
    fromIntegerDefaultEffort :: t -> FromIntegerEffortIndicator t 
    fromIntegerUp :: Integer -> t
    fromIntegerDn :: Integer -> t

propFromIntegerMonotone ::
    (FromInteger t, NumOrd.Comparison t) =>
    t -> Integer -> Integer -> Bool
propFromIntegerMonotone sample i1 i2 
    | i1 > i2 = a1Up NumOrd.> a2Dn
    | i1 <= i2 = a1Dn NumOrd.< a2Up
    | otherwise = True
    where
    a1Dn = fromIntegerDn i1 
    a1Up = fromIntegerUp i1 
    a2Dn = fromIntegerDn i2 
    a2Up = fromIntegerUp i2 
    _ = [sample, a1Dn, a1Up]
    
class ToInteger t where
    type ToIntegerEffortIndicator t
    toIntegerUpEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerDnEff :: ToIntegerEffortIndicator t -> t -> Integer
    toIntegerDefaultEffort :: t -> ToIntegerEffortIndicator t 
    toIntegerUp :: t -> Integer
    toIntegerDn :: t -> Integer
    toIntegerDn a = toIntegerDnEff (toIntegerDefaultEffort a) a
    toIntegerUp a = toIntegerUpEff (toIntegerDefaultEffort a) a

propToIntegerMonotone ::
    (ToInteger t, NumOrd.Comparison t) =>
    t -> t -> t -> Bool
propToIntegerMonotone sample a1 a2 =
    case NumOrd.pCompare a1 a2 of
        Just LT -> i1Dn < i2Up
        Just LEE -> i1Dn <= i2Up
        Just GT -> i1Up > i2Dn
        Just GEE -> i1Up >= i2Dn
        Just EQ -> i1Dn < i2Up
        _ -> True
    where
    i1Dn = toIntegerDn a1 
    i1Up = toIntegerUp a1 
    i2Dn = toIntegerDn a2 
    i2Up = toIntegerUp a2 
    
propToFromInteger ::
    (ToInteger t, FromInteger t, NumOrd.Comparison t) =>
    t -> t -> Bool
propToFromInteger _ a =
    case (aDn NumOrd.<=? a, a NumOrd.<=? aUp) of
       (Just False, _) -> False
       (_, Just False) -> False
       _ -> True
    where
    aDn = fromIntegerDn $ toIntegerDn a 
    aUp = fromIntegerUp $ toIntegerUp a 
    
class FromDouble t where
    type FromDoubleEffortIndicator t
    fromDoubleUpEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDnEff :: FromDoubleEffortIndicator t -> Double -> t
    fromDoubleDefaultEffort :: t -> FromDoubleEffortIndicator t 
    fromDoubleUp :: Double -> t
    fromDoubleDn :: Double -> t

class ToDouble t where
    type ToDoubleEffortIndicator t
    toDoubleUpEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDnEff :: ToDoubleEffortIndicator t -> t -> Double
    toDoubleDefaultEffort :: t -> ToDoubleEffortIndicator t 
    toDoubleUp :: t -> Double
    toDoubleDn :: t -> Double
    toDoubleDn a = toDoubleDnEff (toDoubleDefaultEffort a) a
    toDoubleUp a = toDoubleUpEff (toDoubleDefaultEffort a) a

--class FromRatio t where
--    fromRatioUpEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDnEff :: EffortIndicator -> Ratio Integer -> t
--    fromRatioDefaultEffort :: t -> EffortIndicator 
--    fromRatioUp :: Ratio Integer -> t
--    fromRatioDn :: Ratio Integer -> t
--
--class ToRatio t where
--    toRatioUpEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDnEff :: EffortIndicator -> t -> Ratio Integer
--    toRatioDefaultEffort :: t -> EffortIndicator 
--    toRatioUp :: t -> Ratio Integer
--    toRatioDn :: t -> Ratio Integer
--    toRatioDn a = toRatioDnEff (toRatioDefaultEffort a) a
--    toRatioUp a = toRatioUpEff (toRatioDefaultEffort a) a

