{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.MPFR
    Description :  Interval MPFR utilities and tests  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval MPFR utilities and tests.
-}
module Numeric.AERN.RealArithmetic.Interval.MPFR 
where

import Numeric.AERN.Basics.Interval

import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up

type MI = Interval MPFR

sampleMI :: MI
sampleMI = Interval 0 0

