{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.InPlace.FieldOps
    Description :  rounded in-place arithmetic instances for MPFR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded in-place arithmetic instances for MPFR.
    
    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.InPlace.FieldOps 
(
    setPrec, opMutableNonmutPrec
)
where

import Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps

import Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable

import Numeric.AERN.Misc.Debug

import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Mutable (MMPFR)

import qualified Data.Number.MPFR as M
import qualified Data.Number.MPFR.Mutable as MM

import Numeric.AERN.Basics.Exception
import Control.Exception
import Control.Monad.ST

detectNaNThrow :: String -> MMPFR s -> ST s ()
detectNaNThrow msg aM =
    do
    a <- unsafeReadMutable aM
    if (M.isNaN a) 
       then throw (AERNNaNException $ "MPFR NaN in " ++ msg)
       else return () 

detectNaNDir :: String -> MMPFR s -> M.RoundMode -> ST s ()
detectNaNDir _ aM dir = 
    do
    a <- unsafeReadMutable aM
    case (M.isNaN a, dir) of
       (True, M.Up) -> unsafeWriteMutable aM $ head [1/0, a]
       (True, M.Down) -> unsafeWriteMutable aM $ head [-1/0, a]
       (False, _) -> return ()

setPrec sample prec rM =
    do
    r <- unsafeReadMutable rM
    let _ = [r, sample]
    unsafeWriteMutable rM $ M.set M.Up prec r

opMutable1Unit op name symbol dir sample _ rM dM1 =
        do
        op rM dM1 dir
        d1 <- unsafeReadMutable dM1
        let _ = [d1,sample]
        detectNaNDir (name ++ ": " ++ symbol ++ " " ++ show d1) rM dir

opMutable2Prec op name symbol dir sample prec rM dM1 dM2 =
        do
        setPrec sample prec rM
        op rM dM1 dM2 dir
        d1 <- unsafeReadMutable dM1
        d2 <- unsafeReadMutable dM2
        let _ = [d1,d2,sample]
        detectNaNDir (name ++ ": " ++ show d1 ++ " " ++ symbol ++ show d2 ) rM dir 

opMutableNonmutPrec op name symbol dir sample prec rM dM n =
        do
        setPrec sample prec rM
        op rM dM n dir
        d <- unsafeReadMutable dM
        let _ = [d,sample]
        detectNaNDir (name ++ ": " ++ show d ++ " " ++ symbol ++ show n ) rM dir 

instance RoundedAddInPlace MPFR where
    addUpInPlaceEff = opMutable2Prec MM.add "in-place addition" "+^" M.Up
    addDnInPlaceEff = opMutable2Prec MM.add "in-place addition" "+." M.Down

instance RoundedSubtrInPlace MPFR

instance RoundedAbsInPlace MPFR where
    absUpInPlaceEff = opMutable1Unit MM.absD "in-place abs" "abs" M.Up
    absDnInPlaceEff = opMutable1Unit MM.absD "in-place abs" "abs" M.Down

instance RoundedMultiplyInPlace MPFR where
    multUpInPlaceEff = opMutable2Prec MM.mul "in-place multiplication" "*^" M.Up
    multDnInPlaceEff = opMutable2Prec MM.mul "in-place multiplication" "*." M.Down

instance RoundedPowerNonnegToNonnegIntInPlace MPFR where
    powerNonnegToNonnegIntUpInPlaceEff =
        opMutableNonmutPrec MM.powi "in-place integer power" "^^" M.Up
    powerNonnegToNonnegIntDnInPlaceEff = 
        opMutableNonmutPrec MM.powi "in-place integer power" "^." M.Down

instance RoundedPowerToNonnegIntInPlace MPFR where
    powerToNonnegIntUpInPlaceEff = powerNonnegToNonnegIntUpInPlaceEff
    powerToNonnegIntDnInPlaceEff = powerNonnegToNonnegIntDnInPlaceEff 

instance RoundedRingInPlace MPFR

instance RoundedDivideInPlace MPFR where
    divUpInPlaceEff = opMutable2Prec MM.div "in-place division" "/^" M.Up
    divDnInPlaceEff = opMutable2Prec MM.div "in-place division" "/." M.Down


instance RoundedFieldInPlace MPFR
    