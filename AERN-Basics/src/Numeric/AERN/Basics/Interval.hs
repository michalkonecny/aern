{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval
    Description :  a class of interval types  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.AERN.Basics.Interval where

{-|
    Pairs of endpoints.  A user should not use this type directly
    but use the classes of which this is an instance.
-}
class Interval i where
  type Endpoint i
  getEndpoints :: i -> (Endpoint i, Endpoint i)
  fromEndpoints :: (Endpoint i, Endpoint i) -> i
  mapEndpoints :: (Endpoint i -> Endpoint i) -> (i -> i)
  mapEndpointPair :: ((Endpoint i, Endpoint i) -> (Endpoint i, Endpoint i)) -> (i -> i)
  