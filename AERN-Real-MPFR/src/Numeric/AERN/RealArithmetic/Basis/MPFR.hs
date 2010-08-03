{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR
    Description :  Instances for MPFR as interval endpoints.  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable (indirect FFI)
    
    Instances of MPFR required for serving as interval endpoints,
    namely providing granularity, Comparison, lattice, rounded field and 
    rounded elementary operations.
-}
module Numeric.AERN.RealArithmetic.Basis.MPFR 
(
   M.MPFR,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.ShowInternals,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.Conversion,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.MixedFieldOps,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.Measures,
   module Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps
)
where

import Numeric.AERN.RealArithmetic.Basis.MPFR.ShowInternals
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.MPFR.Conversion
import Numeric.AERN.RealArithmetic.Basis.MPFR.FieldOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.MixedFieldOps
import Numeric.AERN.RealArithmetic.Basis.MPFR.Measures
import Numeric.AERN.RealArithmetic.Basis.MPFR.ExactOps

import Numeric.AERN.Basics.Effort

import qualified Data.Number.MPFR as M

import Test.QuickCheck
import Data.Word

instance Arbitrary M.Precision where
    arbitrary =
        do
        p <- choose (10,1000)
        return (fromInteger $ toInteger (p :: Int))
    
instance EffortIndicator M.Precision where
    effortIncrementVariants p = [2*p + 1]
    effortRepeatIncrement (p1, p2) 
        | p2 > 2*p1 = 2*p2
        | otherwise = p2 + (p2 - p1)
    effortIncrementSequence p =
        map (p +) fibsp2p
        where
        fibsp2p = scanl (+) p (p:fibsp2p)
    
