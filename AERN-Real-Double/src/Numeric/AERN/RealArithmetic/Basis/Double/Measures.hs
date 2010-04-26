{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.Measures
    Description :  distance between Double numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Distance between Double numbers.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.Measures where

import Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.Double.ExactOperations
import Numeric.AERN.RealArithmetic.Basis.Double.RoundedOps

import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.Double

import Numeric.AERN.Basics.CInterval

instance HasDistance Double where
    type Distance Double = DI
    type DistanceEffortIndicator Double = ()
    distanceDefaultEffort _ = ()
    distanceBetweenEff _ d1 d2 =
        let ?addInOutEffort = () :: AddEffortIndicator (Distance Double) in
        absOutEff ((),()) (d2I <-> d1I)
        where
        d1I = fromEndpoints (d1, d1)
        d2I = fromEndpoints (d2, d2)
