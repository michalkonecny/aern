{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double
    Description :  Instances for Double as interval endpoints.  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Instances of Double required for serving as interval endpoints,
    namely providing granularity, poset, lattice, rounded field and 
    rounded elementary operations.
-}
module Numeric.AERN.RealArithmetic.Basis.Double 
(
   module Numeric.AERN.RealArithmetic.Basis.Double.Equality,
   module Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
)
where

import Numeric.AERN.RealArithmetic.Basis.Double.Equality
import Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder

import Numeric.AERN.Basics.Granularity

instance HasGranularity Double where
    type Granularity Double = Int
    getGranularity _ = 53


    