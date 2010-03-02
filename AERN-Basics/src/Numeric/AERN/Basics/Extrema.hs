{-|
    Module      :  Numeric.AERN.Basics.Extrema
    Description :  a class of types that have top and bottom  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Extrema where

{-|
    A type with extrema.
-}
class HasExtrema t where
    top :: t
    bottom :: t
