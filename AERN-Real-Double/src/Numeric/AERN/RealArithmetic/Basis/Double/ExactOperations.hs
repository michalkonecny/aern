{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.Measures
    Description :  one, zero, negation etc for Double numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    One, zero, negation etc for Double numbers.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.ExactOperations where

import Numeric.AERN.RealArithmetic.ExactOperations

instance HasZero Double where
    zero = 0
    
instance HasOne Double where
    one = 1
    
instance Neg Double where
    neg = negate
