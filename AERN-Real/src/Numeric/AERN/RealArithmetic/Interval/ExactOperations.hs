{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ExactOperations
    Description :  exact zero, one and neg for intervals 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Exact zero, one and neg for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.ExactOperations where

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.ExactOperations

instance  (HasZero e) => HasZero (Interval e) where
    zero = Interval zero zero

instance  (HasOne e) => HasOne (Interval e) where
    one = Interval one one

instance  (Neg e) => Neg (Interval e) where
    neg (Interval l h) = Interval (neg h) (neg l)

