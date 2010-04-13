{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double
    Description :  Interval Double utilities and tests  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval Double utilities and tests.
-}
module Numeric.AERN.RealArithmetic.Interval.Double 
where

import Numeric.AERN.RealArithmetic.Basis.Double

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.Interval

import Test.Framework (Test)

type DI = Interval Double

sampleDI :: DI
sampleDI = Interval 0 0

