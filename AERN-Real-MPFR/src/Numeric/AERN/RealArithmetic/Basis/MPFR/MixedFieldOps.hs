{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance RoundedMixedAddEffort MPFR Int
    where
    type MixedAddEffortIndicator MPFR Int = M.Precision
    mixedAddDefaultEffort a _ = M.getPrec a 

instance RoundedMixedAdd MPFR Int
    where
    mixedAddUpEff prec d n = M.addi M.Up prec d n
    mixedAddDnEff prec d n = M.addi M.Down prec d n

instance RoundedMixedMultiplyEffort MPFR Int
    where
    type MixedMultEffortIndicator MPFR Int = M.Precision
    mixedMultDefaultEffort a _ = M.getPrec a

instance RoundedMixedMultiply MPFR Int
    where
    mixedMultUpEff prec d i = M.muli M.Up prec d i
    mixedMultDnEff prec d i = M.muli M.Down prec d i

instance RoundedMixedDivideEffort MPFR Int
    where
    type MixedDivEffortIndicator MPFR Int = M.Precision
    mixedDivDefaultEffort a _ = M.getPrec a

instance RoundedMixedDivide MPFR Int
    where
    mixedDivUpEff prec d i = M.divi M.Up prec d i
    mixedDivDnEff prec d i = M.divi M.Down prec d i

instance RoundedMixedRingEffort MPFR Int
    where
    type MixedRingOpsEffortIndicator MPFR Int = M.Precision
    mixedRingOpsDefaultEffort a _ = M.getPrec a
    mxringEffortAdd _ _ = id
    mxringEffortMult _ _ = id

instance RoundedMixedRing MPFR Int

instance RoundedMixedFieldEffort MPFR Int
    where
    type MixedFieldOpsEffortIndicator MPFR Int = M.Precision
    mixedFieldOpsDefaultEffort a _ = M.getPrec a
    mxfldEffortAdd _ _ = id
    mxfldEffortMult _ _ = id
    mxfldEffortDiv _ _ = id

instance RoundedMixedField MPFR Int

instance RoundedMixedAddEffort MPFR Integer
    where
    type MixedAddEffortIndicator MPFR Integer =
        MixedAddEffortIndicatorByConversion MPFR Integer 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 

instance RoundedMixedAdd MPFR Integer
    where
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiplyEffort MPFR Integer
    where
    type MixedMultEffortIndicator MPFR Integer =
        MixedMultEffortIndicatorByConversion MPFR Integer 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 

instance RoundedMixedMultiply MPFR Integer
    where
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivideEffort MPFR Integer
    where
    type MixedDivEffortIndicator MPFR Integer =
        MixedDivEffortIndicatorByConversion MPFR Integer 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 

instance RoundedMixedDivide MPFR Integer
    where
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRingEffort MPFR Integer
    where
    type MixedRingOpsEffortIndicator MPFR Integer = M.Precision
    mixedRingOpsDefaultEffort a _ = M.getPrec a
    mxringEffortAdd _ _ p = (p,p)
    mxringEffortMult _ _ p = (p,p,())

instance RoundedMixedRing MPFR Integer

instance RoundedMixedFieldEffort MPFR Integer
    where
    type MixedFieldOpsEffortIndicator MPFR Integer = M.Precision
    mixedFieldOpsDefaultEffort a _ = M.getPrec a
    mxfldEffortAdd _ _ p = (p,p)
    mxfldEffortMult _ _ p = (p,p,())
    mxfldEffortDiv _ _ p = (p,p,((),()))

instance RoundedMixedField MPFR Integer



instance RoundedMixedAddEffort MPFR Rational
    where
    type MixedAddEffortIndicator MPFR Rational =
        MixedAddEffortIndicatorByConversion MPFR Rational 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 

instance RoundedMixedAdd MPFR Rational
    where
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiplyEffort MPFR Rational
    where
    type MixedMultEffortIndicator MPFR Rational =
        MixedMultEffortIndicatorByConversion MPFR Rational 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 

instance RoundedMixedMultiply MPFR Rational
    where
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivideEffort MPFR Rational
    where
    type MixedDivEffortIndicator MPFR Rational =
        MixedDivEffortIndicatorByConversion MPFR Rational 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 

instance RoundedMixedDivide MPFR Rational
    where
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

instance RoundedMixedRingEffort MPFR Rational
    where
    type MixedRingOpsEffortIndicator MPFR Rational = M.Precision
    mixedRingOpsDefaultEffort a _ = M.getPrec a
    mxringEffortAdd _ _ p = (p,p)
    mxringEffortMult _ _ p = (p,p,())

instance RoundedMixedRing MPFR Rational

instance RoundedMixedFieldEffort MPFR Rational
    where
    type MixedFieldOpsEffortIndicator MPFR Rational = M.Precision
    mixedFieldOpsDefaultEffort a _ = M.getPrec a
    mxfldEffortAdd _ _ p = (p,p)
    mxfldEffortMult _ _ p = (p,p,())
    mxfldEffortDiv _ _ p = (p,p,((),()))

instance RoundedMixedField MPFR Rational



instance RoundedMixedAddEffort MPFR Double
    where
    type MixedAddEffortIndicator MPFR Double = M.Precision
    mixedAddDefaultEffort a _ = M.getPrec a 

instance RoundedMixedAdd MPFR Double
    where
    mixedAddUpEff prec d i = M.addd M.Up prec d i
    mixedAddDnEff prec d i = M.addd M.Down prec d i

instance RoundedMixedMultiplyEffort MPFR Double
    where
    type MixedMultEffortIndicator MPFR Double = M.Precision
    mixedMultDefaultEffort a _ = M.getPrec a

instance RoundedMixedMultiply MPFR Double
    where
    mixedMultUpEff prec d i = M.muld M.Up prec d i
    mixedMultDnEff prec d i = M.muld M.Down prec d i

instance RoundedMixedDivideEffort MPFR Double
    where
    type MixedDivEffortIndicator MPFR Double = M.Precision
    mixedDivDefaultEffort a _ = M.getPrec a

instance RoundedMixedDivide MPFR Double
    where
    mixedDivUpEff prec d i = M.divd M.Up prec d i
    mixedDivDnEff prec d i = M.divd M.Down prec d i

instance RoundedMixedRingEffort MPFR Double
    where
    type MixedRingOpsEffortIndicator MPFR Double = M.Precision
    mixedRingOpsDefaultEffort a _ = M.getPrec a
    mxringEffortAdd _ _ = id
    mxringEffortMult _ _ = id

instance RoundedMixedRing MPFR Double

instance RoundedMixedFieldEffort MPFR Double
    where
    type MixedFieldOpsEffortIndicator MPFR Double = M.Precision
    mixedFieldOpsDefaultEffort a _ = M.getPrec a
    mxfldEffortAdd _ _ = id
    mxfldEffortMult _ _ = id
    mxfldEffortDiv _ _ = id

instance RoundedMixedField MPFR Double

