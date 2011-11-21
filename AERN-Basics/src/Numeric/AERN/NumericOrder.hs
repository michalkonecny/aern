{-|
    Module      :  Numeric.AERN.NumericOrder
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
module Numeric.AERN.NumericOrder 
(
    module Numeric.AERN.NumericOrder.PartialComparison,
    module Numeric.AERN.NumericOrder.Arbitrary,
    module Numeric.AERN.NumericOrder.Extrema,
    module Numeric.AERN.NumericOrder.RoundedLattice,
    module Numeric.AERN.NumericOrder.InPlace.RoundedLattice,
    module Numeric.AERN.NumericOrder.RefinementRoundedLattice,
    module Numeric.AERN.NumericOrder.InPlace.RefinementRoundedLattice
)
where

import Numeric.AERN.NumericOrder.PartialComparison
import Numeric.AERN.NumericOrder.Arbitrary
import Numeric.AERN.NumericOrder.Extrema
import Numeric.AERN.NumericOrder.RoundedLattice
import Numeric.AERN.NumericOrder.InPlace.RoundedLattice
import Numeric.AERN.NumericOrder.RefinementRoundedLattice
import Numeric.AERN.NumericOrder.InPlace.RefinementRoundedLattice
