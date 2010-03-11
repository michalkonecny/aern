{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.CInterval.Granularity
    Description :  granularity operations for any CInterval instance 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Granularity operations for any 'CInterval' instance.
-}
module Numeric.AERN.Basics.CInterval.Granularity where

import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.Granularity
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

{-|
    Extract granularity from an interval's endpoints.
-}
getGranularityInterval :: 
    (CInterval i, HasGranularity (Endpoint i), 
     NumOrd.Lattice (Granularity (Endpoint i))) =>
    i -> Granularity (Endpoint i)
getGranularityInterval i =
    NumOrd.min (getGranularity l) (getGranularity h)
    where
    (l,h) = getEndpoints i

{-|
    Adjust granularity of an interval's endpoints, rounding outwards.
-}
setMinGranularityOutInterval :: 
    (CInterval i, CanSetGranularityRoundedByNumericOrder (Endpoint i), 
     NumOrd.Lattice (Granularity (Endpoint i))) =>
    (Granularity (Endpoint i)) -> i -> i
setMinGranularityOutInterval gran i =
    mapBothEndpoints (setMinGranularityDn gran) (setMinGranularityUp gran) i

{-|
    Adjust granularity of an interval's endpoints, rounding inwards.
-}
setMinGranularityInInterval :: 
    (CInterval i, CanSetGranularityRoundedByNumericOrder (Endpoint i), 
     NumOrd.Lattice (Granularity (Endpoint i))) =>
    (Granularity (Endpoint i)) -> i -> i
setMinGranularityInInterval gran i =
    mapBothEndpoints (setMinGranularityUp gran) (setMinGranularityDn gran) i

{-|
    Set granularity of an interval's endpoints, rounding outwards.
-}
setGranularityOutInterval :: 
    (CInterval i, CanSetGranularityRoundedByNumericOrder (Endpoint i), 
     NumOrd.Lattice (Granularity (Endpoint i))) =>
    (Granularity (Endpoint i)) -> i -> i
setGranularityOutInterval gran i =
    mapBothEndpoints (setGranularityDn gran) (setGranularityUp gran) i

{-|
    Set granularity of an interval's endpoints, rounding inwards.
-}
setGranularityInInterval :: 
    (CInterval i, CanSetGranularityRoundedByNumericOrder (Endpoint i), 
     NumOrd.Lattice (Granularity (Endpoint i))) =>
    (Granularity (Endpoint i)) -> i -> i
setGranularityInInterval gran i =
    mapBothEndpoints (setGranularityUp gran) (setGranularityDn gran) i
    
