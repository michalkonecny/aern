{-|
    Module      :  Numeric.AERN.Basics.NumericOrder
    Description :  classical and approximate lattices (<,max,min)  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Type classes representing classical as well as approximate 
    Comparisons and lattices with the numerical order notation (<,max,min).
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix NumOrd.
-}
module Numeric.AERN.Basics.NumericOrder 
(
    module Numeric.AERN.Basics.NumericOrder.PartialComparison,
    module Numeric.AERN.Basics.NumericOrder.Arbitrary,
    module Numeric.AERN.Basics.NumericOrder.Extrema,
    module Numeric.AERN.Basics.NumericOrder.RoundedLattice,
    module Numeric.AERN.Basics.NumericOrder.InPlace.RoundedLattice,
    module Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice
)
where

import Numeric.AERN.Basics.NumericOrder.PartialComparison
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.NumericOrder.RoundedLattice
import Numeric.AERN.Basics.NumericOrder.InPlace.RoundedLattice
import Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice
