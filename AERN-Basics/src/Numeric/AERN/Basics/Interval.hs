{-|
    Module      :  Numeric.AERN.Basics.Interval
    Description :  an interval datatype  
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
data Interval e =
    Interval 
    { 
      lowerEndpoint :: e,
      upperEndpoint :: e
    }

getEndpoints :: Interval e -> (e,e)
getEndpoints (Interval lower upper) = (lower, upper)

fromEndpoints :: (e, e) -> Interval e
fromEndpoints (lower, upper) = Interval lower upper
