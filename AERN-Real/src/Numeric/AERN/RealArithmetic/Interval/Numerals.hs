{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Numerals
    Description :  conversions between intervals and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Conversion between intervals and standard numeric types.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.Numerals where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval

instance (ArithUpDn.FromInteger e) => FromInteger (Interval e) where
    type FromIntegerEffortIndicator (Interval e) = ArithUpDn.FromIntegerEffortIndicator e
    fromIntegerDefaultEffort (Interval l h) = ArithUpDn.fromIntegerDefaultEffort l 
    fromIntegerInEff effort n =
        Interval 
           (ArithUpDn.fromIntegerUpEff effort n) 
           (ArithUpDn.fromIntegerDnEff effort n)
    fromIntegerOutEff effort n =
        Interval 
           (ArithUpDn.fromIntegerDnEff effort n) 
           (ArithUpDn.fromIntegerUpEff effort n)
           
instance (ArithUpDn.ToInteger e) => ArithUpDn.ToInteger (Interval e) where
    type ArithUpDn.ToIntegerEffortIndicator (Interval e) = ArithUpDn.ToIntegerEffortIndicator e
    toIntegerDefaultEffort (Interval l h) = ArithUpDn.toIntegerDefaultEffort l 
    toIntegerUpEff effort (Interval l h) = ArithUpDn.toIntegerUpEff effort h
    toIntegerDnEff effort (Interval l h) = ArithUpDn.toIntegerDnEff effort l

instance (ArithUpDn.FromDouble e) => FromDouble (Interval e) where
    type FromDoubleEffortIndicator (Interval e) = ArithUpDn.FromDoubleEffortIndicator e
    fromDoubleDefaultEffort (Interval l h) = ArithUpDn.fromDoubleDefaultEffort l 
    fromDoubleInEff effort n =
        Interval 
           (ArithUpDn.fromDoubleUpEff effort n) 
           (ArithUpDn.fromDoubleDnEff effort n)
    fromDoubleOutEff effort n =
        Interval 
           (ArithUpDn.fromDoubleDnEff effort n) 
           (ArithUpDn.fromDoubleUpEff effort n)
           
instance (ArithUpDn.ToDouble e) => ArithUpDn.ToDouble (Interval e) where
    type ArithUpDn.ToDoubleEffortIndicator (Interval e) = ArithUpDn.ToDoubleEffortIndicator e
    toDoubleDefaultEffort (Interval l h) = ArithUpDn.toDoubleDefaultEffort l 
    toDoubleUpEff effort (Interval l h) = ArithUpDn.toDoubleUpEff effort h
    toDoubleDnEff effort (Interval l h) = ArithUpDn.toDoubleDnEff effort l

           