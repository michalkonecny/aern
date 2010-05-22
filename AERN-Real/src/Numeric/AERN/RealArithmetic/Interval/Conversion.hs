{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Conversion
    Description :  conversions between intervals and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Conversion between intervals and standard numeric types.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.Conversion where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval

instance (ArithUpDn.Convertible Integer e) => Convertible Integer (Interval e) where
    type ConvertEffortIndicator Integer (Interval e) = 
        ArithUpDn.ConvertEffortIndicator Integer e
    convertDefaultEffort i (Interval l h) = ArithUpDn.convertDefaultEffort i l 
    convertInEff effort n =
        Interval 
           (ArithUpDn.convertUpEff effort n) 
           (ArithUpDn.convertDnEff effort n)
    convertOutEff effort n =
        Interval 
           (ArithUpDn.convertDnEff effort n) 
           (ArithUpDn.convertUpEff effort n)
           
instance (ArithUpDn.Convertible e Integer) => 
        ArithUpDn.Convertible (Interval e) Integer where
    type ArithUpDn.ConvertEffortIndicator (Interval e) Integer = 
        ArithUpDn.ConvertEffortIndicator e Integer
    convertDefaultEffort (Interval l h) i = ArithUpDn.convertDefaultEffort l i 
    convertUpEff effort (Interval l h) = ArithUpDn.convertUpEff effort h
    convertDnEff effort (Interval l h) = ArithUpDn.convertDnEff effort l

instance (ArithUpDn.Convertible Double e) => Convertible Double (Interval e) where
    type ConvertEffortIndicator Double (Interval e) = 
        ArithUpDn.ConvertEffortIndicator Double e
    convertDefaultEffort d (Interval l h) = ArithUpDn.convertDefaultEffort d l 
    convertInEff effort d =
        Interval 
           (ArithUpDn.convertUpEff effort d) 
           (ArithUpDn.convertDnEff effort d)
    convertOutEff effort d =
        Interval 
           (ArithUpDn.convertDnEff effort d) 
           (ArithUpDn.convertUpEff effort d)
           
instance (ArithUpDn.Convertible e Double) => 
        ArithUpDn.Convertible (Interval e) Double where
    type ArithUpDn.ConvertEffortIndicator (Interval e) Double = 
        ArithUpDn.ConvertEffortIndicator e Double
    convertDefaultEffort (Interval l h) d = ArithUpDn.convertDefaultEffort l d 
    convertUpEff effort (Interval l h) = ArithUpDn.convertUpEff effort h
    convertDnEff effort (Interval l h) = ArithUpDn.convertDnEff effort l

instance (ArithUpDn.Convertible Rational e) => Convertible Rational (Interval e) where
    type ConvertEffortIndicator Rational (Interval e) = 
        ArithUpDn.ConvertEffortIndicator Rational e
    convertDefaultEffort d (Interval l h) = ArithUpDn.convertDefaultEffort d l 
    convertInEff effort d =
        Interval 
           (ArithUpDn.convertUpEff effort d) 
           (ArithUpDn.convertDnEff effort d)
    convertOutEff effort d =
        Interval 
           (ArithUpDn.convertDnEff effort d) 
           (ArithUpDn.convertUpEff effort d)
           
instance (ArithUpDn.Convertible e Rational) => 
        ArithUpDn.Convertible (Interval e) Rational where
    type ArithUpDn.ConvertEffortIndicator (Interval e) Rational = 
        ArithUpDn.ConvertEffortIndicator e Rational
    convertDefaultEffort (Interval l h) d = ArithUpDn.convertDefaultEffort l d 
    convertUpEff effort (Interval l h) = ArithUpDn.convertUpEff effort h
    convertDnEff effort (Interval l h) = ArithUpDn.convertDnEff effort l
    