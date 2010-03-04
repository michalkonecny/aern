{-|
    Module      :  Numeric.AERN.Basics.Interval.GenericImplementation.NumericOrder
    Description :  interval instances of numeric-ordered structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Interval.GenericImplementation.NumericOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Interval
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd


instance (NumOrd.SemidecidablePoset e) => (SemidecidableEq (Interval e))
    where
    maybeEqual = maybeEqualIntervals
    maybeEqualDefaultEffort = maybeEqualDefaultEffortIntervals

maybeEqualDefaultEffortIntervals (Interval l h) =
    zipWith max
        (maybeEqualDefaultEffort l)
        (maybeEqualDefaultEffort h)
        
maybeEqualIntervals effort i1 i2 = 
    case (c l1 l2, c l1 h2, c h1 l2, c h1 h2) of
        (Just EQ, Just EQ, Just EQ, _) -> Just True
        (Just LT, Just LT, Just LT, Just LT) -> Just False  
        (Just GT, Just GT, Just GT, Just GT) -> Just False
        (Just NC, Just NC, Just NC, Just NC) -> Just False
        _ -> Nothing
    where
    c = NumOrd.maybeCompare effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2
     
    
instance (NumOrd.SemidecidablePoset e) => (NumOrd.SemidecidablePoset (Interval e))
    where
    maybeCompare = maybeCompareIntervals
    maybeCompareDefaultEffort = maybeCompareDefaultEffortIntervals
        
maybeCompareDefaultEffortIntervals (Interval l h) =
    zipWith max
        (NumOrd.maybeCompareDefaultEffort l)
        (NumOrd.maybeCompareDefaultEffort h)
        
maybeCompareIntervals effort i1 i2 = 
    case (c l1 l2, c l1 h2, c h1 l2, c h1 h2) of
        (Just EQ, Just EQ, Just EQ, _) -> Just EQ
        (Just LT, Just LT, Just LT, Just LT) -> Just LT  
        (Just GT, Just GT, Just GT, Just GT) -> Just GT
        (Just NC, Just NC, Just NC, Just NC) -> Just NC
        _ -> Nothing
    where
    c = NumOrd.maybeCompare effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2

-- TODO: add instances for Lattice and RoundedLattice
