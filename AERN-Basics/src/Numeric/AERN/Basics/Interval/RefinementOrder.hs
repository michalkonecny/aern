{-|
    Module      :  Numeric.AERN.Basics.Interval.RefinementOrder
    Description :  interval instances of refinement-ordered structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Interval.RefinementOrder where

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Interval.NumericOrder
import Numeric.AERN.Basics.CInterval.RefinementOrder

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

instance (NumOrd.Poset e) => (RefOrd.Poset (Interval e))
    where
    compare = compareIntervalRef
    
instance (NumOrd.SemidecidablePoset e) => (RefOrd.SemidecidablePoset (Interval e))
    where
    maybeCompare = maybeCompareIntervalRef
    maybeCompareDefaultEffort = maybeCompareDefaultEffortIntervalRef

-- TODO: add instances for Basis, OuterRoundedBasis, InnerRoundedBasis
--       Lattice, OuterRoundedLattice, InnerRoundedLattice    