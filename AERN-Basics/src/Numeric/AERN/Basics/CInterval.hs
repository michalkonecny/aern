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
-}
module Numeric.AERN.Basics.CInterval where

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

{-|
    A class of types that represent some intervals.
-}
class CInterval i where
    type Endpoint i
    getEndpoints :: i -> (Endpoint i, Endpoint i)
    fromEndpoints :: (Endpoint i, Endpoint i) -> i
    mapEndpoints :: (Endpoint i -> Endpoint i) -> (i -> i)
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
