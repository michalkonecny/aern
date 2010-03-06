{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Extrema
    Description :  types that have least and highest elements  
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
class (HasLeast t, HasHighest t) => HasExtrema t

{-|
    A type with a least element.
-}
class HasLeast t where
    least :: t

{-|
    A type with a highest element.
-}
class HasHighest t where
    highest :: t
    