{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.UpDnConversion
    Description :  conversions between intervals and standard numeric types, rounded up/dn
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Conversion between intervals and standard numeric types.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.UpDnConversion 
()
where

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Numeric.AERN.Basics.Interval

instance (Convertible e t) => 
        Convertible (Interval e) t where
    type ConvertEffortIndicator (Interval e) t = 
        ConvertEffortIndicator e t
    convertDefaultEffort (Interval sampleE _) i = convertDefaultEffort sampleE i 
    convertUpEff effort sampleT (Interval _ r) = convertUpEff effort sampleT r
    convertDnEff effort sampleT (Interval l _) = convertDnEff effort sampleT l

---- moved to Numeric.AERN.RealArithmetic.Interval.Double:
--instance 
--    Convertible Double (Interval Double)
--    where
--    type ConvertEffortIndicator Double (Interval Double) = 
--        ()
--    convertDefaultEffort _ _ = () 
--    convertUpEff _ _ x =
--        Just $ Interval x x
--    convertDnEff _ _ x =
--        Just $ Interval x x


