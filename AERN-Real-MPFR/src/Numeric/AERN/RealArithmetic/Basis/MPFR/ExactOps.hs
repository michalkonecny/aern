{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
    Description :  one, zero, negation etc for MPFR numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    One, zero, negation etc for MPFR numbers.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps where

import Numeric.AERN.RealArithmetic.Basis.MPFR.Mutable

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up
import qualified Data.Number.MPFR.Mutable as MM

instance HasZero MPFR where
    zero = 0
    
instance HasOne MPFR where
    one = 1
    
instance HasInfinities MPFR where
    plusInfinity = 1/0
    minusInfinity = -1/0
    excludesPlusInfinity a = (a /= plusInfinity)
    excludesMinusInfinity a = (a /= minusInfinity)
    
instance Neg MPFR where
    neg = negate

instance NegInPlace MPFR where
    negInPlace _ r = 
        do
        MM.neg r r M.Near
        return ()


    