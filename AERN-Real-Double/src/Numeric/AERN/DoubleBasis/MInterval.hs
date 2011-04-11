
module Numeric.AERN.DoubleBasis.MInterval 

where

import Numeric.AERN.RealArithmetic.Basis.Double
  (MDouble(..))

import Numeric.AERN.Basics.Interval
  (Interval(..),getEndpoints,fromEndpoints)

import Numeric.AERN.RealArithmetic.NumericOrderRounding

type MDI s = Interval (MDouble s)



