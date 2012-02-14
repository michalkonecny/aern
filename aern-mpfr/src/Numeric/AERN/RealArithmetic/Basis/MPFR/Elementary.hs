{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.Elementary
    Description :  rounded elementary ops based on MPFR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded elementary ops based on MPFR.
    
    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.Elementary 
()
where

import Numeric.AERN.RealArithmetic.Basis.MPFR.Effort

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up ()

instance RoundedExponentiationEffort MPFR where
    type ExpEffortIndicator MPFR = M.Precision 
    expDefaultEffort = M.getPrec

instance RoundedExponentiation MPFR where
    expUpEff prec d1 = 
        M.exp M.Up prec d1
    expDnEff prec d1 =
        M.exp M.Down prec d1

instance RoundedSquareRootEffort MPFR where
    type SqrtEffortIndicator MPFR = M.Precision 
    sqrtDefaultEffort = M.getPrec

instance RoundedSquareRoot MPFR where
    sqrtUpEff prec d1 = 
        M.sqrt M.Up prec d1
    sqrtDnEff prec d1 =
        M.sqrt M.Down prec d1


