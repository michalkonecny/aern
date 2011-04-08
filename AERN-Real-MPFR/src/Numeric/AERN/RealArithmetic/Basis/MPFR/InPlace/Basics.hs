{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.Mutable
    Description :  MPFR mutable version 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    MPFR mutable version.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.InPlace.Basics 
(
)
where

import Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.NumericOrder

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)

import qualified Data.Number.MPFR.Mutable as MM
import Data.Number.MPFR.Mutable (MMPFR)

instance CanBeMutable MPFR where
    type Mutable MPFR = MMPFR
    makeMutable a = MM.thaw a
    unsafeMakeMutable a = MM.unsafeThaw a
    writeMutable = MM.writeMMPFR 
    unsafeWriteMutable = MM.unsafeWriteMMPFR
    readMutable = MM.freeze 
    unsafeReadMutable = MM.unsafeFreeze 
    
instance NegInPlace MPFR where
    negInPlace _ r a = 
        do
        MM.neg r a M.Near
        return ()
        
instance RoundedLatticeInPlace MPFR
    where
    maxUpInPlaceEff = maxUpInPlaceEffFromPure
    maxDnInPlaceEff = maxDnInPlaceEffFromPure
    minUpInPlaceEff = minUpInPlaceEffFromPure
    minDnInPlaceEff = minDnInPlaceEffFromPure

    