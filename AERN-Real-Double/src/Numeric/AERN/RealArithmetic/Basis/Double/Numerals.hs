{-# LANGUAGE TypeFamilies #-}
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

instance FromInteger Double where
    type FromIntegerEffortIndicator Double = ()
    fromIntegerDefaultEffort _ = ()
    fromIntegerUpEff _ n 
       | ndn >= n = dn
       | otherwise = dnUp
       where
       ndn = toInteger n
       dn = fromInteger n
       (m, e) = decodeFloat dn
       dnUp = encodeFloat (m + 1) e 
    fromIntegerDnEff _ n 
       | ndn <= n = dn
       | otherwise = dnDn
       where
       ndn = toInteger n
       dn = fromInteger n
       (m, e) = decodeFloat dn
       dnDn = encodeFloat (m - 1) e 
    
instance ToInteger Double where
    type ToIntegerEffortIndicator Double = ()
    toIntegerDefaultEffort _ = ()
    toIntegerUpEff _ d = ceiling d
    toIntegerDnEff _ d = floor d

instance FromDouble Double where
    type FromDoubleEffortIndicator Double = ()
    fromDoubleDefaultEffort _ = ()
    fromDoubleUpEff _ d = d
    fromDoubleDnEff _ d = d

instance ToDouble Double where
    type ToDoubleEffortIndicator Double = ()
    toDoubleDefaultEffort _ = ()
    toDoubleUpEff _ d = d
    toDoubleDnEff _ d = d

    