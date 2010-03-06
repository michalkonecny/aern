{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.Extrema
    Description :  types that have top and bottom  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.RefinementOrder.Extrema where

{-|
    A type with extrema.
-}
class HasExtrema t where
    top :: t
    bottom :: t
    
(⊤) :: (HasExtrema t) => t   
(⊤) = top
(⊥) :: (HasExtrema t) => t   
(⊥) = bottom
