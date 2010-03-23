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
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.NumericOrder where

import Numeric.AERN.Basics.Interval.Basics

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.CInterval.NumericOrder

instance (NumOrd.SemidecidableComparison e) => (NumOrd.SemidecidableComparison (Interval e))
    where
    maybeCompareEff = maybeCompareEffInterval
    maybeCompareDefaultEffort = maybeCompareDefaultEffortInterval
        
instance (NumOrd.Comparison e) => (NumOrd.Comparison (Interval e))
        
instance (NumOrd.Lattice e) => (NumOrd.Lattice (Interval e))
    where
    min = minInterval 
    max = maxInterval 

instance (NumOrd.HasLeast e) => (NumOrd.HasLeast (Interval e))
    where
    least = leastInterval
    
instance (NumOrd.HasHighest e) => (NumOrd.HasHighest (Interval e))
    where
    highest = highestInterval
    
instance (NumOrd.HasExtrema e) => (NumOrd.HasExtrema (Interval e))

instance (NumOrd.ArbitraryOrderedTuple e) => NumOrd.ArbitraryOrderedTuple (Interval e) where
   arbitraryTupleRelatedBy = arbitraryIntervalTupleNumericallyRelatedBy
