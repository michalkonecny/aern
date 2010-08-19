{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing MPFR and another type
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing MPFR and another type.
    
    This module is hidden and reexported via its parent MPFR. 
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.MixedFieldOps where

import Numeric.AERN.RealArithmetic.Basis.MPFR.Conversion
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up
import qualified Data.Number.MPFR.Mutable as MM

instance RoundedMixedAdd MPFR Int
    where
    type MixedAddEffortIndicator MPFR Int = M.Precision
    mixedAddDefaultEffort _ _ = 100 
    mixedAddUpEff prec d n = M.addi M.Up prec d n
    mixedAddDnEff prec d n = M.addi M.Down prec d n

instance RoundedMixedMultiply MPFR Int
    where
    type MixedMultEffortIndicator MPFR Int = M.Precision
    mixedMultDefaultEffort _ _ = 100
    mixedMultUpEff prec d i = M.muli M.Up prec d i
    mixedMultDnEff prec d i = M.muli M.Down prec d i

instance RoundedMixedDivide MPFR Int
    where
    type MixedDivEffortIndicator MPFR Int = M.Precision
    mixedDivDefaultEffort _ _ = 100
    mixedDivUpEff prec d i = M.divi M.Up prec d i
    mixedDivDnEff prec d i = M.divi M.Down prec d i

instance RoundedMixedRing MPFR Int
instance RoundedMixedField MPFR Int
    where
    type MixedFieldOpsEffortIndicator MPFR Int = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ = id
    mxfldEffortMult _ _ = id
    mxfldEffortDiv _ _ = id

instance RoundedMixedAdd MPFR Integer
    where
    type MixedAddEffortIndicator MPFR Integer =
        MixedAddEffortIndicatorByConversion MPFR Integer 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply MPFR Integer
    where
    type MixedMultEffortIndicator MPFR Integer =
        MixedMultEffortIndicatorByConversion MPFR Integer 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide MPFR Integer
    where
    type MixedDivEffortIndicator MPFR Integer =
        MixedDivEffortIndicatorByConversion MPFR Integer 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing MPFR Integer
instance RoundedMixedField MPFR Integer
    where
    type MixedFieldOpsEffortIndicator MPFR Integer = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ p = (p,p)
    mxfldEffortMult _ _ p = (p,p,())
    mxfldEffortDiv _ _ p = (p,p,((),()))

instance RoundedMixedAdd MPFR Rational
    where
    type MixedAddEffortIndicator MPFR Rational =
        MixedAddEffortIndicatorByConversion MPFR Rational 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply MPFR Rational
    where
    type MixedMultEffortIndicator MPFR Rational =
        MixedMultEffortIndicatorByConversion MPFR Rational 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide MPFR Rational
    where
    type MixedDivEffortIndicator MPFR Rational =
        MixedDivEffortIndicatorByConversion MPFR Rational 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing MPFR Rational
instance RoundedMixedField MPFR Rational
    where
    type MixedFieldOpsEffortIndicator MPFR Rational = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ p = (p,p)
    mxfldEffortMult _ _ p = (p,p,())
    mxfldEffortDiv _ _ p = (p,p,((),()))

instance RoundedMixedAdd MPFR Double
    where
    type MixedAddEffortIndicator MPFR Double = M.Precision
    mixedAddDefaultEffort _ _ = 100 
    mixedAddUpEff prec d i = M.addd M.Up prec d i
    mixedAddDnEff prec d i = M.addd M.Down prec d i

instance RoundedMixedMultiply MPFR Double
    where
    type MixedMultEffortIndicator MPFR Double = M.Precision
    mixedMultDefaultEffort _ _ = 100
    mixedMultUpEff prec d i = M.muld M.Up prec d i
    mixedMultDnEff prec d i = M.muld M.Down prec d i

instance RoundedMixedDivide MPFR Double
    where
    type MixedDivEffortIndicator MPFR Double = M.Precision
    mixedDivDefaultEffort _ _ = 100
    mixedDivUpEff prec d i = M.divd M.Up prec d i
    mixedDivDnEff prec d i = M.divd M.Down prec d i

instance RoundedMixedRing MPFR Double
instance RoundedMixedField MPFR Double
    where
    type MixedFieldOpsEffortIndicator MPFR Double = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ = id
    mxfldEffortMult _ _ = id
    mxfldEffortDiv _ _ = id

