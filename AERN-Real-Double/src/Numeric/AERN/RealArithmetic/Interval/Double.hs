{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double
    Description :  convenient export for Interval Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenient export for Interval Double with its instances and tests.
-}
module Numeric.AERN.RealArithmetic.Interval.Double 
(
   module Numeric.AERN.RealArithmetic.Interval.Double.Basics,
   module Numeric.AERN.RealArithmetic.Interval.Double.NumericOrder,
   module Numeric.AERN.RealArithmetic.Interval.Double.RefinementOrder,
   testsDIConsistencyFlip
)
where

import Numeric.AERN.RealArithmetic.Interval.Double.Basics
import Numeric.AERN.RealArithmetic.Interval.Double.NumericOrder
import Numeric.AERN.RealArithmetic.Interval.Double.RefinementOrder

import Numeric.AERN.Basics.Interval

testsDIConsistencyFlip = testsIntervalConsistencyFlip "DI" sampleDI
