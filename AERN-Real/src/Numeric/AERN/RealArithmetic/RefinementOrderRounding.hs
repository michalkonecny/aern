{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RefinementOrderRounding
    Description :  common arithmetical operations rounded in/out  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common arithmetical operations rounded in/out.
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix ArithInOut.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding
(
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary
)
where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

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
     NumOrd.PartialComparison t, NumOrd.RefinementRoundedLattice t,
     RefOrd.PartialComparison t, RefOrd.RoundedLattice t, 
     Convertible Int t, ArithUpDn.Convertible t Int,
     Convertible Integer t, ArithUpDn.Convertible t Integer,  
     Convertible Double t, ArithUpDn.Convertible t Double,  
     Convertible Rational t, ArithUpDn.Convertible t Rational,  
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
    rrNumCompEffort :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.PartialCompareEffortIndicator t)
    rrMinmaxEffort :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.MinmaxEffortIndicator t)
    rrCompEffort :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.PartialCompareEffortIndicator t)
    rrJoinMeetOutEffort :: t -> (RoundedRealEffortIndicator t) -> (RefOrd.JoinMeetOutEffortIndicator t)
    rrJoinMeetInEffort :: t -> (RoundedRealEffortIndicator t) -> (RefOrd.JoinMeetInEffortIndicator t)
    rrToIntEffort :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Int)
    rrFromIntEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Int t)
    rrToIntegerEffort :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Integer)
    rrFromIntegerEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Integer t)
    rrToDoubleEffort :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Double)
    rrFromDoubleEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Double t)
    rrToRationalEffort :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Rational)
    rrFromRationalEffort :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Rational t)
    rrAbsEffort :: t -> (RoundedRealEffortIndicator t) -> (AbsEffortIndicator t)
    rrFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (FieldOpsEffortIndicator t)
    rrIntMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Int t)
    rrIntegerMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Integer t)
    rrDoubleMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Double t)
    rrRationalMixedFieldEffort :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator Rational t)
