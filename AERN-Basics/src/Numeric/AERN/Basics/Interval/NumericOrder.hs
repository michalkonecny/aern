{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.NumericOrder
    Description :  interval instances of numeric-ordered structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval instances of numeric-ordered structures.
-}
module Numeric.AERN.Basics.Interval.NumericOrder where

import Numeric.AERN.Basics.Equality
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval.NumericOrder

instance (NumOrd.SemidecidablePoset e) => (SemidecidableEq (Interval e))
    where
    maybeEqual = maybeEqualInterval
    maybeEqualDefaultEffort = maybeEqualDefaultEffortInterval

instance (NumOrd.SemidecidablePoset e) => (NumOrd.SemidecidablePoset (Interval e))
    where
    maybeCompare = maybeCompareInterval
    maybeCompareDefaultEffort = maybeCompareDefaultEffortInterval
        
instance (NumOrd.Lattice e) => (NumOrd.Lattice (Interval e))
    where
    min = minInterval 
    max = maxInterval 
