{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.CInterval
    Description :  a class of interval datatypes  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    A class of interval datatypes and associated functions related to granularity and extrema.
-}
module Numeric.AERN.Basics.CInterval where

import Numeric.AERN.Basics.Granularity
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

{-|
    A class of types that represent some intervals.
-}
class CInterval i where
    type Endpoint i
    getEndpoints :: i -> (Endpoint i, Endpoint i)
    fromEndpoints :: (Endpoint i, Endpoint i) -> i
    mapEndpoints :: (Endpoint i -> Endpoint i) -> (i -> i)
    mapBothEndpoints :: (Endpoint i -> Endpoint i) -> (Endpoint i -> Endpoint i) -> (i -> i)
    mapEndpointPair :: ((Endpoint i, Endpoint i) -> (Endpoint i, Endpoint i)) -> (i -> i)
    
propIntervalConsistent :: 
    (CInterval i, NumOrd.Poset (Endpoint i)) => 
    i -> Bool
propIntervalConsistent i =
    l NumOrd.<= h
    where
    (l,h) = getEndpoints i
    
propIntervalAntiConsistent :: 
    (CInterval i, NumOrd.Poset (Endpoint i)) => 
    i -> Bool 
propIntervalAntiConsistent i =
    h NumOrd.<= l
    where
    (l,h) = getEndpoints i
    
propIntervalConsistentAntiConsistent :: 
    (CInterval i, NumOrd.Poset (Endpoint i)) => 
    i -> Bool 
propIntervalConsistentAntiConsistent i =
    l NumOrd.<= h || h NumOrd.<= l
    where
    (l,h) = getEndpoints i

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
    
leastInterval ::
     (CInterval i, NumOrd.HasLeast (Endpoint i)) => i
leastInterval = fromEndpoints (NumOrd.least, NumOrd.least)
    
highestInterval ::
     (CInterval i, NumOrd.HasHighest (Endpoint i)) => i
highestInterval = fromEndpoints (NumOrd.highest, NumOrd.highest)

bottomInterval ::
     (CInterval i, NumOrd.HasExtrema (Endpoint i)) => i
bottomInterval = fromEndpoints (NumOrd.least, NumOrd.highest)

topInterval ::
     (CInterval i, NumOrd.HasExtrema (Endpoint i)) => i
topInterval = fromEndpoints (NumOrd.highest, NumOrd.least)
    
