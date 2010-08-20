{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.InPlace.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing MPFR and another type
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetical operations mixing MPFR and another type.
    
    This module is hidden and reexported via its grand-parent MPFR. 
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.InPlace.MixedFieldOps where

import Numeric.AERN.RealArithmetic.Basis.MPFR.MixedFieldOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.InPlace.FieldOps

import Numeric.AERN.RealArithmetic.Basis.MPFR.Conversion
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Mutable (MMPFR)

import qualified Data.Number.MPFR as M
import qualified Data.Number.MPFR.Mutable as MM

instance RoundedMixedAddInPlace MPFR Int
    where
    mixedAddUpInPlaceEff =
        opMutableNonmutPrec MM.addi "in-place int addition" "+^|" M.Up
    mixedAddDnInPlaceEff =
        opMutableNonmutPrec MM.addi "in-place int addition" "+^|" M.Down

instance RoundedMixedMultiplyInPlace MPFR Int
    where
    mixedMultUpInPlaceEff =
        opMutableNonmutPrec MM.muli "in-place int multiplication" "*^|" M.Up
    mixedMultDnInPlaceEff =
        opMutableNonmutPrec MM.muli "in-place int multiplication" "*^|" M.Down

instance RoundedMixedDivideInPlace MPFR Int
    where
    mixedDivUpInPlaceEff =
        opMutableNonmutPrec MM.divi "in-place int division" "/^|" M.Up
    mixedDivDnInPlaceEff =
        opMutableNonmutPrec MM.divi "in-place int division" "/^|" M.Down

instance RoundedMixedRingInPlace MPFR Int
instance RoundedMixedFieldInPlace MPFR Int

instance RoundedMixedAddInPlace MPFR Integer
    where
    mixedAddUpInPlaceEff = mixedAddUpInPlaceEffByConversion
    mixedAddDnInPlaceEff = mixedAddDnInPlaceEffByConversion

instance RoundedMixedMultiplyInPlace MPFR Integer
instance RoundedMixedDivideInPlace MPFR Integer

instance RoundedMixedRingInPlace MPFR Integer
instance RoundedMixedFieldInPlace MPFR Integer

instance RoundedMixedAddInPlace MPFR Rational
    where
    mixedAddUpInPlaceEff = mixedAddUpInPlaceEffByConversion
    mixedAddDnInPlaceEff = mixedAddDnInPlaceEffByConversion

instance RoundedMixedMultiplyInPlace MPFR Rational
instance RoundedMixedDivideInPlace MPFR Rational

instance RoundedMixedRingInPlace MPFR Rational
instance RoundedMixedFieldInPlace MPFR Rational

instance RoundedMixedAddInPlace MPFR Double
    where
    mixedAddUpInPlaceEff =
        opMutableNonmutPrec MM.addd "in-place Double addition" "+^|" M.Up
    mixedAddDnInPlaceEff =
        opMutableNonmutPrec MM.addd "in-place Double addition" "+^|" M.Down

instance RoundedMixedMultiplyInPlace MPFR Double
    where
    mixedMultUpInPlaceEff =
        opMutableNonmutPrec MM.muld "in-place Double multiplication" "*^|" M.Up
    mixedMultDnInPlaceEff =
        opMutableNonmutPrec MM.muld "in-place Double multiplication" "*^|" M.Down

instance RoundedMixedDivideInPlace MPFR Double
    where
    mixedDivUpInPlaceEff =
        opMutableNonmutPrec MM.divd "in-place Double division" "/^|" M.Up
    mixedDivDnInPlaceEff =
        opMutableNonmutPrec MM.divd "in-place Double division" "/^|" M.Down

instance RoundedMixedRingInPlace MPFR Double
instance RoundedMixedFieldInPlace MPFR Double

