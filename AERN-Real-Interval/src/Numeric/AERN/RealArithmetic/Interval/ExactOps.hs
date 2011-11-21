{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ExactOps
    Description :  exact zero, one and neg for intervals 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Exact zero, one and neg for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.ExactOps 
()
where

import Numeric.AERN.Basics.Interval
import qualified Numeric.AERN.NumericOrder as NumOrd 
import Numeric.AERN.RealArithmetic.ExactOps

instance  (HasZero e, NumOrd.PartialComparison e) => HasZero (Interval e) where
    zero = Interval zero zero

instance  (HasOne e) => HasOne (Interval e) where
    one = Interval one one

instance (HasInfinities e) => HasInfinities (Interval e) where
    plusInfinity = Interval plusInfinity plusInfinity
    minusInfinity = Interval minusInfinity minusInfinity
    excludesPlusInfinity (Interval l r) = excludesPlusInfinity r
    excludesMinusInfinity (Interval l r) = excludesMinusInfinity l

instance  (Neg e) => Neg (Interval e) where
    neg (Interval l r) = Interval (neg r) (neg l)

