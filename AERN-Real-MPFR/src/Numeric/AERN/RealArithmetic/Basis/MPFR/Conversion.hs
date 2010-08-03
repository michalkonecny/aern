{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.Conversion
    Description :  conversions between MPFR and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions between MPFR and standard numeric types.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.Conversion where

import Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps

import Numeric.AERN.RealArithmetic.NumericOrderRounding
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Exception
import Control.Exception

import Data.Ratio

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up
import qualified Data.Number.MPFR.Mutable as MM


instance Convertible Integer MPFR where
    type ConvertEffortIndicator Integer MPFR = M.Precision
    convertDefaultEffort _ _ = 100
    convertUpEff prec n = Just $ M.fromIntegerA M.Up prec n 
    convertDnEff prec n = Just $ M.fromIntegerA M.Down prec n

instance Convertible Int MPFR where
    type ConvertEffortIndicator Int MPFR = M.Precision
    convertDefaultEffort _ _ = 100
    convertUpEff prec n = Just $ M.fromInt M.Up prec n 
    convertDnEff prec n = Just $ M.fromInt M.Down prec n

instance Convertible MPFR Integer where
    type ConvertEffortIndicator MPFR Integer = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d 
        | M.isInfinite d = Nothing
        | otherwise = Just $ ceiling d
    convertDnEff _ d 
        | M.isInfinite d = Nothing
        | otherwise = Just $ floor d

instance Convertible MPFR Int where
    type ConvertEffortIndicator MPFR Int = ()
    convertDefaultEffort _ _ = ()
    convertUpEff effort d =
        case mdUpInteger of
            Nothing -> Nothing
            Just dUpInteger
                | dUpInteger > (toInteger intMax) -> Nothing
                | otherwise -> Just $ fromInteger dUpInteger
        where
        mdUpInteger = convertUpEff effort d
        intMax = maxBound :: Int
    convertDnEff effort d =
        case mdDnInteger of
            Nothing -> Nothing
            Just dDnInteger
                | dDnInteger < (toInteger intMin) -> Nothing
                | otherwise -> Just $ fromInteger dDnInteger
        where
        mdDnInteger = convertDnEff effort d
        intMin = minBound :: Int

instance Convertible MPFR Double where
    type ConvertEffortIndicator MPFR Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d
        | d == plusInfinity = Just (1/0) 
        | d == minusInfinity = Just (-1/0)
        | otherwise =
            case M.toDouble2exp M.Up d of
                (s1,e) ->
                   case encodeFloat (fst $ decodeFloat s1) (e - 53) of
                       d2 | d2 == -(1/0) -> Just dblMinBound
                       d2 | d2 == 0 && d > 0 -> Just dblEpsilon
                       d2 -> Just d2 
    convertDnEff _ d
        | d == plusInfinity = Just (1/0) 
        | d == minusInfinity = Just (-1/0)
        | otherwise =
            case M.toDouble2exp M.Down d of
                (s1,e) ->
                   case encodeFloat (fst $ decodeFloat s1) (e - 53) of
                       d2 | d2 == (1/0) -> Just dblMaxBound
                       d2 | d2 == 0 && d < 0 -> Just (- dblEpsilon)
                       d2 -> Just d2 


(dblSigMaxBound, dblExpMaxBound) = decodeFloat $ (0/1 :: Double)

dblMinBound, dblMaxBound :: Double
dblMinBound = encodeFloat (negate $ dblSigMaxBound - 1) dblExpMaxBound
dblMaxBound = - dblMinBound

dblEpsilon :: Double
dblEpsilon = encodeFloat 1 (-1074)

instance Convertible Double MPFR where
    type ConvertEffortIndicator Double MPFR = M.Precision
    convertDefaultEffort _ _ = 100
    convertUpEff prec d = Just $ M.fromDouble M.Up prec d 
    convertDnEff prec d = Just $ M.fromDouble M.Down prec d

instance Convertible Rational MPFR where
    type ConvertEffortIndicator Rational MPFR = M.Precision
    convertDefaultEffort _ _ = 100
    convertUpEff prec r 
        | r < 0 = fmap negate (positiveRational2MPFRDn prec (- r))
        | otherwise = positiveRational2MPFRUp prec r
    convertDnEff prec r 
        | r < 0 = fmap negate (positiveRational2MPFRUp prec (- r))
        | otherwise = positiveRational2MPFRDn prec r
        
positiveRational2MPFRUp prec r =        
        case (convertUpEff prec $ numerator r, convertDnEff prec $ denominator r) of
            (Just num, Just den) -> Just $ divUpEff prec num den
            _ -> Nothing
positiveRational2MPFRDn prec r =        
        case (convertDnEff prec $ numerator r, convertUpEff prec $ denominator r) of
            (Just num, Just den) -> Just $ divDnEff prec num den
            _ -> Nothing

instance Convertible MPFR Rational where
    type ConvertEffortIndicator MPFR Rational = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d
        | M.isInfinite d = Nothing
        | otherwise = Just $ toRational d
    convertDnEff eff d = convertUpEff eff d

