{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing Double and another type
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing Double and another type.
    
    This module is hidden and reexported via its parent Double. 
-}

module Numeric.AERN.RealArithmetic.Basis.Double.MixedFieldOps where

import Numeric.AERN.RealArithmetic.Basis.Double.Conversion
import Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.Double.FieldOps

import Numeric.AERN.RealArithmetic.NumericOrderRounding

instance RoundedMixedAdd Integer Double
    where
    type MixedAddEffortIndicator Integer Double =
        MixedAddEffortIndicatorByConversion Integer Double 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Integer Double
    where
    type MixedMultEffortIndicator Integer Double =
        MixedMultEffortIndicatorByConversion Integer Double 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Integer Double
    where
    type MixedDivEffortIndicator Integer Double =
        MixedDivEffortIndicatorByConversion Integer Double 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedAdd Rational Double
    where
    type MixedAddEffortIndicator Rational Double =
        MixedAddEffortIndicatorByConversion Rational Double 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Rational Double
    where
    type MixedMultEffortIndicator Rational Double =
        MixedMultEffortIndicatorByConversion Rational Double 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Rational Double
    where
    type MixedDivEffortIndicator Rational Double =
        MixedDivEffortIndicatorByConversion Rational Double 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedAdd Double Double
    where
    type MixedAddEffortIndicator Double Double = () 
    mixedAddDefaultEffort _ _ = () 
    mixedAddUpEff _ a b = addUpEff () a b 
    mixedAddDnEff _ a b = addDnEff () a b

instance RoundedMixedMultiply Double Double
    where
    type MixedMultEffortIndicator Double Double = () 
    mixedMultDefaultEffort _ _ = () 
    mixedMultUpEff _ a b = multUpEff () a b 
    mixedMultDnEff _ a b = multDnEff () a b

instance RoundedMixedDivide Double Double
    where
    type MixedDivEffortIndicator Double Double = () 
    mixedDivDefaultEffort _ _ = () 
    mixedDivUpEff _ a b = divUpEff () a b 
    mixedDivDnEff _ a b = divDnEff () a b

