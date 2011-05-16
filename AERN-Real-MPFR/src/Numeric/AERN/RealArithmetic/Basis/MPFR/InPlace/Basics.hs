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
where

import Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.NumericOrder

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)

import qualified Data.Number.MPFR.Mutable as MM

instance CanBeMutable MPFR where
    data Mutable MPFR s = MMPFR { unMMPFR :: MM.MMPFR s }
    makeMutable a = do { aM <- MM.thaw a; return $ MMPFR aM } 
    unsafeMakeMutable a = do { aM <- MM.unsafeThaw a; return $ MMPFR aM }
    writeMutable (MMPFR m) = MM.writeMMPFR m 
    unsafeWriteMutable (MMPFR m) = MM.unsafeWriteMMPFR m
    readMutable (MMPFR m) = MM.freeze m 
    unsafeReadMutable (MMPFR m) = MM.unsafeFreeze m 
    
instance NegInPlace MPFR where
    negInPlace (MMPFR r) (MMPFR a) = 
        do
        MM.neg r a M.Near
        return ()
        
instance RoundedLatticeInPlace MPFR
    where
    maxUpInPlaceEff = maxUpInPlaceEffFromPure
    maxDnInPlaceEff = maxDnInPlaceEffFromPure
    minUpInPlaceEff = minUpInPlaceEffFromPure
    minDnInPlaceEff = minDnInPlaceEffFromPure

    