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
    
    A class of interval datatypes.
-}
module Numeric.AERN.Basics.CInterval where

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
    
propCIntervalConsistent :: 
    (CInterval i, NumOrd.Poset (Endpoint i)) => 
    i -> Bool
propCIntervalConsistent i =
    l NumOrd.<= h
    where
    (l,h) = getEndpoints i
    
propCIntervalAntiConsistent :: 
    (CInterval i, NumOrd.Poset (Endpoint i)) => 
    i -> Bool 
propCIntervalAntiConsistent i =
    h NumOrd.<= l
    where
    (l,h) = getEndpoints i
    
propCIntervalConsistentAntiConsistent :: 
    (CInterval i, NumOrd.Poset (Endpoint i)) => 
    i -> Bool 
propCIntervalConsistentAntiConsistent i =
    l NumOrd.<= h || h NumOrd.<= l
    where
    (l,h) = getEndpoints i


