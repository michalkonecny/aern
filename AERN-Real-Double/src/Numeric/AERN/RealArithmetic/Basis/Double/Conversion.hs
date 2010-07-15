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
       | ndn >= n = dn
       | otherwise = dnUp
       where
       dn = fromInteger n
       ndn = floor dn
       (m, e) = decodeFloat dn
       dnUp = encodeFloat (m + 1) e
    convertDnEff _ n
       | ndn <= n = dn
       | otherwise = dnDn
       where
       dn = fromInteger n
       ndn = ceiling dn
       (m, e) = decodeFloat dn
       dnDn = encodeFloat (m - 1) e

instance Convertible Double Integer where
    type ConvertEffortIndicator Double Integer = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d 
        | isInfinite d =
            throw  $ 
                AERNException $ 
                    "cannot convert" ++ show d ++ " to an integer"
        | otherwise = ceiling d
    convertDnEff _ d 
        | isInfinite d =
            throw  $ 
                AERNException $ 
                    "cannot convert" ++ show d ++ " to an integer"
        | otherwise = floor d

instance Convertible Double Double where
    type ConvertEffortIndicator Double Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d = d
    convertDnEff _ d = d

instance Convertible Rational Double where
    type ConvertEffortIndicator Rational Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ r
       | rdr >= r = dr
       | otherwise = drUp
       where
       rdr = toRational dr
       dr = fromRational r
       (m, e) = decodeFloat dr
       drUp = encodeFloat (m + 1) e
    convertDnEff _ r
       | rdr <= r = dr
       | otherwise = drDn
       where
       rdr = toRational dr
       dr = fromRational r
       (m, e) = decodeFloat dr
       drDn = encodeFloat (m - 1) e

instance Convertible Double Rational where
    type ConvertEffortIndicator Double Rational = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d
        | isInfinite d =
            throw  $ 
                AERNException $ 
                    "cannot convert" ++ show d ++ " to a rational"
        | otherwise = toRational d
    convertDnEff eff d = convertUpEff eff d

