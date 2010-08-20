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

instance RoundedAdd MPFR where
    type AddEffortIndicator MPFR = M.Precision 
    addDefaultEffort _ = 100
    addUpEff prec d1 d2 = 
        detectNaNUp ("addition " ++ show d1 ++ " +^ " ++ show d2 ) $ 
            M.add M.Up prec d1 d2
    addDnEff prec d1 d2 =
        detectNaNDn ("addition " ++ show d1 ++ " +. " ++ show d2 ) $ 
            M.add M.Down prec d1 d2
    
instance RoundedSubtr MPFR

instance RoundedAbs MPFR where
    type AbsEffortIndicator MPFR = ()
    absDefaultEffort _ = ()
    absDnEff _ = abs
    absUpEff _ = abs

instance RoundedMultiply MPFR where
    type MultEffortIndicator MPFR = M.Precision 
    multDefaultEffort _ = 100
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

instance RoundedRing MPFR

instance RoundedPowerNonnegToNonnegInt MPFR where
    type PowerNonnegToNonnegIntEffortIndicator MPFR = M.Precision
    powerNonnegToNonnegIntDefaultEffort _ = 100
    powerNonnegToNonnegIntUpEff prec x n =
        M.powi M.Up prec x n 
    powerNonnegToNonnegIntDnEff prec x n = 
        M.powi M.Down prec x n 

instance RoundedPowerToNonnegInt MPFR where
    type PowerToNonnegIntEffortIndicator MPFR = M.Precision 
    powerToNonnegIntDefaultEffort _ = 100 
    powerToNonnegIntUpEff prec x n =
        M.powi M.Up prec x n 
    powerToNonnegIntDnEff prec x n = 
        M.powi M.Down prec x n 

instance RoundedDivide MPFR where
    type DivEffortIndicator MPFR = M.Precision 
    divDefaultEffort _ = 100
    divUpEff prec d1 d2 = 
        detectNaNUp ("division " ++ show d1 ++ " *^ " ++ show d2 ) $ 
            M.div M.Up prec d1 d2
    divDnEff prec d1 d2 = 
        detectNaNDn ("division " ++ show d1 ++ " *. " ++ show d2 ) $ 
            M.div M.Down prec d1 d2

instance RoundedField MPFR
    where
    type FieldOpsEffortIndicator MPFR = M.Precision
    fieldOpsDefaultEffort _ = 100
    fldEffortAdd _ = id
    fldEffortMult _ = id
    fldEffortPow _ = id
    fldEffortDiv _ = id

    

    