{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Numeric.AERN.RealArithmetic.Basis.Double.ExactOps
import Numeric.AERN.RealArithmetic.Basis.Double.RoundedOps

import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.Double

import Numeric.AERN.Basics.CInterval

instance HasDistance Double where
    type Distance Double = DI
    type DistanceEffortIndicator Double = AddEffortIndicator DI
    distanceDefaultEffort d = addDefaultEffort (sampleDI :: DI)
    distanceBetweenEff effort d1 d2 =
--        | d1 == 1/0 && d2 == 1/0 = zero 
--          -- distance between two infinities is zero (beware: distance not continuous at infinities!)  
--        | d1 == -1/0 && d2 == -1/0 = zero
--        | otherwise =
            let ?addInOutEffort = effort :: AddEffortIndicator (Distance Double) in
            absOutEff ((),()) (d2I <-> d1I)
            where
            d1I = fromEndpoints (d1, d1)
            d2I = fromEndpoints (d2, d2)
