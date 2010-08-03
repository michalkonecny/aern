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

instance RoundedMixedAdd Int MPFR
    where
    type MixedAddEffortIndicator Int MPFR = M.Precision
    mixedAddDefaultEffort _ _ = 100 
    mixedAddUpEff prec i d = M.addi M.Up prec d i
    mixedAddDnEff prec i d = M.addi M.Down prec d i

instance RoundedMixedMultiply Int MPFR
    where
    type MixedMultEffortIndicator Int MPFR = M.Precision
    mixedMultDefaultEffort _ _ = 100
    mixedMultUpEff prec i d = M.muli M.Up prec d i
    mixedMultDnEff prec i d = M.muli M.Down prec d i

instance RoundedMixedDivide Int MPFR
    where
    type MixedDivEffortIndicator Int MPFR = M.Precision
    mixedDivDefaultEffort _ _ = 100
    mixedDivUpEff prec d i = M.divi M.Up prec d i
    mixedDivDnEff prec d i = M.divi M.Down prec d i

instance RoundedMixedRing Int MPFR
instance RoundedMixedField Int MPFR
    where
    type MixedFieldOpsEffortIndicator Int MPFR = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ = id
    mxfldEffortMult _ _ = id
    mxfldEffortDiv _ _ = id

instance RoundedMixedAdd Integer MPFR
    where
    type MixedAddEffortIndicator Integer MPFR =
        MixedAddEffortIndicatorByConversion Integer MPFR 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Integer MPFR
    where
    type MixedMultEffortIndicator Integer MPFR =
        MixedMultEffortIndicatorByConversion Integer MPFR 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Integer MPFR
    where
    type MixedDivEffortIndicator Integer MPFR =
        MixedDivEffortIndicatorByConversion Integer MPFR 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing Integer MPFR
instance RoundedMixedField Integer MPFR
    where
    type MixedFieldOpsEffortIndicator Integer MPFR = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ p = (p,p)
    mxfldEffortMult _ _ p = (p,p,())
    mxfldEffortDiv _ _ p = (p,p,((),()))

instance RoundedMixedAdd Rational MPFR
    where
    type MixedAddEffortIndicator Rational MPFR =
        MixedAddEffortIndicatorByConversion Rational MPFR 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Rational MPFR
    where
    type MixedMultEffortIndicator Rational MPFR =
        MixedMultEffortIndicatorByConversion Rational MPFR 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Rational MPFR
    where
    type MixedDivEffortIndicator Rational MPFR =
        MixedDivEffortIndicatorByConversion Rational MPFR 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRing Rational MPFR
instance RoundedMixedField Rational MPFR
    where
    type MixedFieldOpsEffortIndicator Rational MPFR = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ p = (p,p)
    mxfldEffortMult _ _ p = (p,p,())
    mxfldEffortDiv _ _ p = (p,p,((),()))

instance RoundedMixedAdd Double MPFR
    where
    type MixedAddEffortIndicator Double MPFR = M.Precision
    mixedAddDefaultEffort _ _ = 100 
    mixedAddUpEff prec i d = M.addd M.Up prec d i
    mixedAddDnEff prec i d = M.addd M.Down prec d i

instance RoundedMixedMultiply Double MPFR
    where
    type MixedMultEffortIndicator Double MPFR = M.Precision
    mixedMultDefaultEffort _ _ = 100
    mixedMultUpEff prec i d = M.muld M.Up prec d i
    mixedMultDnEff prec i d = M.muld M.Down prec d i

instance RoundedMixedDivide Double MPFR
    where
    type MixedDivEffortIndicator Double MPFR = M.Precision
    mixedDivDefaultEffort _ _ = 100
    mixedDivUpEff prec d i = M.divd M.Up prec d i
    mixedDivDnEff prec d i = M.divd M.Down prec d i

instance RoundedMixedRing Double MPFR
instance RoundedMixedField Double MPFR
    where
    type MixedFieldOpsEffortIndicator Double MPFR = M.Precision
    mixedFieldOpsDefaultEffort _ _ = 100
    mxfldEffortAdd _ _ = id
    mxfldEffortMult _ _ = id
    mxfldEffortDiv _ _ = id

