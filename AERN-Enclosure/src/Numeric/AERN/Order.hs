{-|
    Module      :  Numeric.AERN.Order
    Description :  semi-decidable posets and lattices 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Order where

data PartialOrdering = Equal | Less | Greater | Incomparable 

class (Eq t) => Poset t where
    compare :: t -> t -> PartialOrdering

class ApproxEq t where
    maybeEqual :: t -> t -> Maybe Bool

class (ApproxEq t) => ApproxPoset t where
    maybeCompare :: t -> t -> Maybe Ordering
