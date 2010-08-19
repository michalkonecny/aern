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

instance RoundedMixedAdd Double Int
    where
    type MixedAddEffortIndicator Double Int =
        MixedAddEffortIndicatorByConversion Double Int 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Double Int
    where
    type MixedMultEffortIndicator Double Int =
        MixedMultEffortIndicatorByConversion Double Int 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Double Int
    where
    type MixedDivEffortIndicator Double Int =
        MixedDivEffortIndicatorByConversion Double Int 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing Double Int
instance RoundedMixedField Double Int
    where
    type MixedFieldOpsEffortIndicator Double Int = ()
    mixedFieldOpsDefaultEffort _ _ = ()
    mxfldEffortAdd _ _ _ = ((),())
    mxfldEffortMult _ _ _ = ((),(),())
    mxfldEffortDiv _ _ _ = ((),(),((),()))

instance RoundedMixedAdd Double Integer
    where
    type MixedAddEffortIndicator Double Integer =
        MixedAddEffortIndicatorByConversion Double Integer 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Double Integer
    where
    type MixedMultEffortIndicator Double Integer =
        MixedMultEffortIndicatorByConversion Double Integer 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Double Integer
    where
    type MixedDivEffortIndicator Double Integer =
        MixedDivEffortIndicatorByConversion Double Integer 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing Double Integer
instance RoundedMixedField Double Integer
    where
    type MixedFieldOpsEffortIndicator Double Integer = ()
    mixedFieldOpsDefaultEffort _ _ = ()
    mxfldEffortAdd _ _ _ = ((),())
    mxfldEffortMult _ _ _ = ((),(),())
    mxfldEffortDiv _ _ _ = ((),(),((),()))

instance RoundedMixedAdd Double Rational
    where
    type MixedAddEffortIndicator Double Rational =
        MixedAddEffortIndicatorByConversion Double Rational 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Double Rational
    where
    type MixedMultEffortIndicator Double Rational =
        MixedMultEffortIndicatorByConversion Double Rational 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Double Rational
    where
    type MixedDivEffortIndicator Double Rational =
        MixedDivEffortIndicatorByConversion Double Rational 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing Double Double
instance RoundedMixedField Double Double
    where
    type MixedFieldOpsEffortIndicator Double Double = ()
    mixedFieldOpsDefaultEffort _ _ = ()
    mxfldEffortAdd _ _ _ = ()
    mxfldEffortMult _ _ _ = ()
    mxfldEffortDiv _ _ _ = ()

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

instance RoundedMixedRing Double Rational
instance RoundedMixedField Double Rational
    where
    type MixedFieldOpsEffortIndicator Double Rational = ()
    mixedFieldOpsDefaultEffort _ _ = ()
    mxfldEffortAdd _ _ _ = ((),())
    mxfldEffortMult _ _ _ = ((),(),())
    mxfldEffortDiv _ _ _ = ((),(),((),()))
