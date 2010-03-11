{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.Granularity
    Description :  interval instances of granularity classes 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval instances of granularity classes.
-}
module Numeric.AERN.Basics.Interval.Granularity where

import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval.Granularity

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

instance (HasGranularity e, NumOrd.Lattice (Granularity e)) => 
         HasGranularity (Interval e)
    where
    type Granularity (Interval e) = Granularity e
    getGranularity = getGranularityInterval

instance (CanSetGranularityRoundedByNumericOrder e, NumOrd.Lattice (Granularity e)) => 
         CanSetGranularityRoundedByRefinementOrder (Interval e)
    where
    setGranularityOut = setGranularityOutInterval
    setGranularityIn = setGranularityInInterval
    setMinGranularityOut = setMinGranularityOutInterval
    setMinGranularityIn = setMinGranularityInInterval
