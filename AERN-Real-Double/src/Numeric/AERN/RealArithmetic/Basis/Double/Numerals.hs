{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.Numerals
    Description :  conversions between Double and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Conversions between Double and standard numeric types.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.Numerals where

import Numeric.AERN.RealArithmetic.NumericOrderRounding

instance Convertible Integer Double where
    type ConvertEffortIndicator Integer Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ n
       | ndn >= n = dn
       | otherwise = dnUp
       where
       ndn = toInteger n
       dn = fromInteger n
       (m, e) = decodeFloat dn
       dnUp = encodeFloat (m + 1) e 
    convertDnEff _ n 
       | ndn <= n = dn
       | otherwise = dnDn
       where
       ndn = toInteger n
       dn = fromInteger n
       (m, e) = decodeFloat dn
       dnDn = encodeFloat (m - 1) e 
    
instance Convertible Double Integer where
    type ConvertEffortIndicator Double Integer = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d = ceiling d
    convertDnEff _ d = floor d

instance Convertible Double Double where
    type ConvertEffortIndicator Double Double = ()
    convertDefaultEffort _ _ = ()
    convertUpEff _ d = d
    convertDnEff _ d = d

