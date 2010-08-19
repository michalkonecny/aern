{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding
    Description :  common arithmetical operations rounded up/down  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common arithmetical operations rounded up/down.
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix ArithUpDn.
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding 
(
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace
)
where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

{-|
   An aggregate class collecting together all functionality
   normally expected from up/down rounded approximations to
   real numbers such as the floating point numbers.
   
   It also provides a single aggregate effort indicator type
   from which effort indicators for all the rounded operations can
   be extracted.
-}
class 
    (HasZero t, HasOne t, HasInfinities t, Neg t,
     NumOrd.PartialComparison t, NumOrd.RoundedLattice t,
     Convertible Int t, Convertible t Int,  
     Convertible Integer t, Convertible t Integer,  
     Convertible Double t, Convertible t Double,  
     Convertible Rational t, Convertible t Rational,  
     RoundedAbs t, 
     RoundedField t,
     RoundedMixedField Int t, 
     RoundedMixedField Integer t, 
     RoundedMixedField Double t, 
     RoundedMixedField Rational t) => 
    RoundedReal t
    where
    type RoundedRealEffortIndicator t
    roundedRealDefaultEffort :: t -> RoundedRealEffortIndicator t
    rrCompEffort :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.PartialCompareEffortIndicator t)
    rrMinmaxEffort :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.MinmaxEffortIndicator t)
    rrToIntEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator t Int)
    rrFromIntEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Int t)
    rrToIntegerEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator t Integer)
    rrFromIntegerEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Integer t)
    rrToDoubleEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator t Double)
    rrFromDoubleEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Double t)
    rrToRationalEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator t Rational)
    rrFromRationalEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Rational t)
    rrAbsEffort :: t -> (RoundedRealEffortIndicator t) -> (AbsEffortIndicator t)
    rrFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (FieldOpsEffortIndicator t)
    rrIntMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Int t)
    rrIntegerMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Integer t)
    rrDoubleMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Double t)
    rrRationalMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Rational t)
     
    