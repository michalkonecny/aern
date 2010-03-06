{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Extrema
    Description :  types that have top and bottom  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.NumericOrder.Extrema where

{-|
    A type with extrema.
-}
class HasExtrema t where
    highest :: t
    least :: t
