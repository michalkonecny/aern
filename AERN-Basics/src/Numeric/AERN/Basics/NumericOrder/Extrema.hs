{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Extrema
    Description :  types that have least and highest elements  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Types that have least and highest elements.
    
    This module is hidden and reexported via its parent NumericOrder. 
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
    