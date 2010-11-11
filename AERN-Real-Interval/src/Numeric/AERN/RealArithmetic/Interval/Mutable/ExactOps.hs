{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps
    Description :  neg for mutable intervals 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Exact neg for mutable intervals. 
    
    This module is hidden and reexported via its parent Interval.Mutable. 
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps where

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.ExactOps

instance (NegInPlace e, Neg e) => NegInPlace (Interval e)
    where
    negInPlace (Interval sample _) (MInterval lM hM) =
        do
        negInPlace sample lM
        negInPlace sample hM
        swapMutable sample lM hM
          
