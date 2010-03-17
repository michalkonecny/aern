{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder
    Description :  classical and approximate domain bases and lattices (&#8849;⊑,&#8851;⊓,&#8852;⊔)  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Type classes representing classical as well as approximate 
    Comparisons, domain bases and lattices with the refinement order notation 
    (&#8849;⊑,&#8851;⊓,&#8852;⊔).
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix RefOrd.
-}
module Numeric.AERN.Basics.RefinementOrder 
(
    module Numeric.AERN.Basics.RefinementOrder.SemidecidableComparison,
    module Numeric.AERN.Basics.RefinementOrder.Comparison,
    module Numeric.AERN.Basics.RefinementOrder.Arbitrary,
    module Numeric.AERN.Basics.RefinementOrder.Basis,
    module Numeric.AERN.Basics.RefinementOrder.RoundedBasis,
    module Numeric.AERN.Basics.RefinementOrder.Lattice,
    module Numeric.AERN.Basics.RefinementOrder.RoundedLattice,
    module Numeric.AERN.Basics.RefinementOrder.Extrema
)
where

import Numeric.AERN.Basics.RefinementOrder.SemidecidableComparison
import Numeric.AERN.Basics.RefinementOrder.Comparison
import Numeric.AERN.Basics.RefinementOrder.Arbitrary
import Numeric.AERN.Basics.RefinementOrder.Basis
import Numeric.AERN.Basics.RefinementOrder.RoundedBasis
import Numeric.AERN.Basics.RefinementOrder.Lattice
import Numeric.AERN.Basics.RefinementOrder.RoundedLattice
import Numeric.AERN.Basics.RefinementOrder.Extrema
