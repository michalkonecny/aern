{-# LANGUAGE TypeFamilies #-}
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

{-|
    A class of types that represent some intervals.
-}
class CInterval i where
    type Endpoint i
    getEndpoints :: i -> (Endpoint i, Endpoint i)
    fromEndpoints :: (Endpoint i, Endpoint i) -> i
    mapEndpoints :: (Endpoint i -> Endpoint i) -> (i -> i)
    mapEndpointPair :: ((Endpoint i, Endpoint i) -> (Endpoint i, Endpoint i)) -> (i -> i)
