{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.Conversion
    Description :  conversions between Double and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions between Double and standard numeric types.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.Conversion where

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Numeric.AERN.Basics.Exception
import Control.Exception

instance Convertible Integer Double where
    type ConvertEffortIndicator Integer Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ n
       | ndn >= n = Just dn
       | otherwise = Just dnUp
       where
       dn = fromInteger n
       ndn = floor dn
       (m, e) = decodeFloat dn
       dnUp = encodeFloat (m + 1) e
    convertDnEff _ n
       | ndn <= n = Just dn
       | otherwise = Just dnDn
       where
       dn = fromInteger n
       ndn = ceiling dn
       (m, e) = decodeFloat dn
       dnDn = encodeFloat (m - 1) e

instance Convertible Int Double where
    type ConvertEffortIndicator Int Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff effort n =
       convertUpEff effort (toInteger n)
    convertDnEff effort n =
       convertDnEff effort (toInteger n)

instance Convertible Double Integer where
    type ConvertEffortIndicator Double Integer = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d 
        | isInfinite d = Nothing
        | otherwise = Just $ ceiling d
    convertDnEff _ d 
        | isInfinite d = Nothing
        | otherwise = Just $ floor d

instance Convertible Double Int where
    type ConvertEffortIndicator Double Int = ()
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

instance Convertible Double Double where
    type ConvertEffortIndicator Double Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d = Just d
    convertDnEff _ d = Just d

instance Convertible Rational Double where
    type ConvertEffortIndicator Rational Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ r
       | rdr >= r = Just dr
       | otherwise = Just drUp
       where
       rdr = toRational dr
       dr = fromRational r
       (m, e) = decodeFloat dr
       drUp = encodeFloat (m + 1) e
    convertDnEff _ r
       | rdr <= r = Just dr
       | otherwise = Just drDn
       where
       rdr = toRational dr
       dr = fromRational r
       (m, e) = decodeFloat dr
       drDn = encodeFloat (m - 1) e

instance Convertible Double Rational where
    type ConvertEffortIndicator Double Rational = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d
        | isInfinite d = Nothing
        | otherwise = Just $ toRational d
    convertDnEff eff d = convertUpEff eff d

