{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.Mutable
    Description :  one, zero, negation etc for MPFR numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    One, zero, negation etc for MPFR numbers.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.MPFR.Mutable where

import Numeric.AERN.Basics.Mutable

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
    
    
    