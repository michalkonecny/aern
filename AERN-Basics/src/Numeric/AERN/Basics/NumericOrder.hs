{-|
    Module      :  Numeric.AERN.Basics.NumericOrder
    Description :  classical and approximate lattices (<,max,min)  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Type classes representing classical as well as approximate 
    posets and lattices with the numerical order notation (<,max,min).
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix NumOrd.
-}
module Numeric.AERN.Basics.NumericOrder 
(
    module Numeric.AERN.Basics.NumericOrder.Extrema,
    module Numeric.AERN.Basics.NumericOrder.Poset,
    module Numeric.AERN.Basics.NumericOrder.Lattice,
    module Numeric.AERN.Basics.NumericOrder.SemidecidablePoset,
    module Numeric.AERN.Basics.NumericOrder.RoundedLattice
)
where

import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.NumericOrder.Poset
import Numeric.AERN.Basics.NumericOrder.Lattice
import Numeric.AERN.Basics.NumericOrder.SemidecidablePoset
import Numeric.AERN.Basics.NumericOrder.RoundedLattice
