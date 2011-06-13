{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps
    Description :  rounded arithmetic instances for MPFR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded arithmetic instances for MPFR.
    
    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps 
()
where

import Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.MPFR.Utilities

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Numeric.AERN.Basics.Effort

import Numeric.AERN.Misc.Debug

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up
import qualified Data.Number.MPFR.Mutable as MM

instance RoundedAddEffort MPFR where
    type AddEffortIndicator MPFR = M.Precision 
    addDefaultEffort = M.getPrec

instance RoundedAdd MPFR where
    addUpEff prec d1 d2 = 
        detectNaNUp ("addition " ++ show d1 ++ " +^ " ++ show d2 ) $ 
            M.add M.Up prec d1 d2
    addDnEff prec d1 d2 =
        detectNaNDn ("addition " ++ show d1 ++ " +. " ++ show d2 ) $ 
            M.add M.Down prec d1 d2
    
instance RoundedSubtr MPFR

instance RoundedAbsEffort MPFR where
    type AbsEffortIndicator MPFR = ()
    absDefaultEffort _ = ()

instance RoundedAbs MPFR where
    absDnEff _ = abs
    absUpEff _ = abs

instance RoundedMultiplyEffort MPFR where
    type MultEffortIndicator MPFR = M.Precision 
    multDefaultEffort = M.getPrec

instance RoundedMultiply MPFR where
    multUpEff prec d1 d2 = 
--        unsafePrintReturn
--        (
--            "MPFR multiplication UP with prec = " ++ show prec 
--            ++ " " ++ show d1 ++ " * " ++ show d2 ++ " = " 
--        ) $
        detectNaNUp ("multiplication " ++ show d1 ++ " *^ " ++ show d2 ) $ 
            M.mul M.Up prec d1 d2
    multDnEff prec d1 d2 = 
--        unsafePrintReturn
--        (
--            "MPFR multiplication DOWN with prec = " ++ show prec 
--            ++ " " ++ show d1 ++ " * " ++ show d2 ++ " = " 
--        ) $
        detectNaNDn ("multiplication " ++ show d1 ++ " *. " ++ show d2 ) $ 
            M.mul M.Down prec d1 d2


instance RoundedPowerNonnegToNonnegIntEffort MPFR where
    type PowerNonnegToNonnegIntEffortIndicator MPFR = M.Precision
    powerNonnegToNonnegIntDefaultEffort = M.getPrec

instance RoundedPowerNonnegToNonnegInt MPFR where
    powerNonnegToNonnegIntUpEff prec x n =
        M.powi M.Up prec x n 
    powerNonnegToNonnegIntDnEff prec x n = 
        M.powi M.Down prec x n 

instance RoundedPowerToNonnegIntEffort MPFR where
    type PowerToNonnegIntEffortIndicator MPFR = M.Precision 
    powerToNonnegIntDefaultEffort = M.getPrec 

instance RoundedPowerToNonnegInt MPFR where
    powerToNonnegIntUpEff prec x n =
        M.powi M.Up prec x n 
    powerToNonnegIntDnEff prec x n = 
        M.powi M.Down prec x n 

instance RoundedDivideEffort MPFR where
    type DivEffortIndicator MPFR = M.Precision 
    divDefaultEffort = M.getPrec

instance RoundedDivide MPFR where
    divUpEff prec d1 d2 = 
        detectNaNUp ("division " ++ show d1 ++ " *^ " ++ show d2 ) $ 
            M.div M.Up prec d1 d2
    divDnEff prec d1 d2 = 
        detectNaNDn ("division " ++ show d1 ++ " *. " ++ show d2 ) $ 
            M.div M.Down prec d1 d2

instance RoundedRingEffort MPFR
    where
    type RingOpsEffortIndicator MPFR = M.Precision
    ringOpsDefaultEffort = M.getPrec
    ringEffortAdd _ = id
    ringEffortMult _ = id
    ringEffortPow _ = id

instance RoundedRing MPFR

instance RoundedFieldEffort MPFR
    where
    type FieldOpsEffortIndicator MPFR = M.Precision
    fieldOpsDefaultEffort = M.getPrec
    fldEffortAdd _ = id
    fldEffortMult _ = id
    fldEffortPow _ = id
    fldEffortDiv _ = id

instance RoundedField MPFR
    

    