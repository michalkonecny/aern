{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval
    Description :  instances of arithmetic classes for Intervals  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Instances of arithmetic classes for Intervals.
-}

module Numeric.AERN.RealArithmetic.Interval
(
    module Numeric.AERN.RealArithmetic.Interval.ExactOps,
    module Numeric.AERN.RealArithmetic.Interval.Measures,
    module Numeric.AERN.RealArithmetic.Interval.Conversion,
    module Numeric.AERN.RealArithmetic.Interval.FieldOps,
    module Numeric.AERN.RealArithmetic.Interval.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.Interval.SpecialConst,
    module Numeric.AERN.RealArithmetic.Interval.Floating
)
where

import Numeric.AERN.RealArithmetic.Interval.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Measures
import Numeric.AERN.RealArithmetic.Interval.Conversion
import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps
import Numeric.AERN.RealArithmetic.Interval.SpecialConst
import Numeric.AERN.RealArithmetic.Interval.Floating

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Interval

instance (HasLegalValues e) => HasLegalValues (Interval e) where
    isLegal (Interval l h) = isLegal l && isLegal h 

