{-|
    Module      :  Numeric.AERN.Basics.Interval.RefinementOrder
    Description :  interval instances of refinement-ordered structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval instances of refinement-ordered structures.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.RefinementOrder where

import Numeric.AERN.Basics.Interval.Basics
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

    
instance (NumOrd.HasExtrema e) => (RefOrd.HasTop (Interval e))
    where
    top = topInterval
    
instance (NumOrd.HasExtrema e) => (RefOrd.HasBottom (Interval e))
    where
    bottom = bottomInterval

-- TODO: add instances for Basis, OuterRoundedBasis, InnerRoundedBasis
--       Lattice, OuterRoundedLattice, InnerRoundedLattice    