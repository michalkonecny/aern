{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double.Basics
    Description :  type synonym and basic tests for Interval Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Type synonym and basic tests for Interval Double.
-}
module Numeric.AERN.RealArithmetic.Interval.Double.Basics 
(
   DI, sampleDI
)
where

import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.Interval

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

type DI = Interval Double

sampleDI :: DI
sampleDI = Interval 0 0