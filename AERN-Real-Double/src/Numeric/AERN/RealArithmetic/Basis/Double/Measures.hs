{-# LANGUAGE TypeFamilies #-}
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

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Interval.Double

instance HasDistance Double where
    type Distance Double = DI
    distanceBetween d1 d2 =
--        abs (d2I <-> d1I)
--        where
--        d1I = fromEndpoints (d1, d1)
--        d2I = fromEndpoints (d2, d2)
        error "distanceBetween not defined yet for Double"  
