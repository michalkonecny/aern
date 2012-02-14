{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps
    Description :  neg for mutable intervals 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Exact neg for mutable intervals. 
    
    This module is hidden and reexported via its parent Interval.Mutable. 
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

instance (NegInPlace e, Neg e) => NegInPlace (Interval e)
    where
    negInPlace (MInterval lRes hRes) (MInterval lM hM) =
        do
        temp <- cloneMutable hRes
        negInPlace temp lM -- mind potential aliasing hRes - hM
        negInPlace lRes hM
        assignMutable hRes temp
