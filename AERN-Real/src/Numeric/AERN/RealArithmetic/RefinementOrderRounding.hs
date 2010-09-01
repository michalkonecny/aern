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
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace,
    RoundedReal(..)
)
where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace

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
     RoundedMixedField t Int, 
     RoundedMixedField t Integer, 
     RoundedMixedField t Double, 
     RoundedMixedField t Rational) => 
    RoundedReal t
    where
    type RoundedRealEffortIndicator t
    roundedRealDefaultEffort :: t -> RoundedRealEffortIndicator t
    rrEffortNumComp :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.PartialCompareEffortIndicator t)
    rrEffortMinmax :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.MinmaxEffortIndicator t)
    rrEffortComp :: t -> (RoundedRealEffortIndicator t) -> (NumOrd.PartialCompareEffortIndicator t)
    rrEffortJoinMeetOut :: t -> (RoundedRealEffortIndicator t) -> (RefOrd.JoinMeetOutEffortIndicator t)
    rrEffortJoinMeetIn :: t -> (RoundedRealEffortIndicator t) -> (RefOrd.JoinMeetInEffortIndicator t)
    rrEffortToInt :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Int)
    rrEffortFromInt :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Int t)
    rrEffortToInteger :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Integer)
    rrEffortFromInteger :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Integer t)
    rrEffortToDouble :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Double)
    rrEffortFromDouble :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Double t)
    rrEffortToRational :: t -> (RoundedRealEffortIndicator t) -> (ArithUpDn.ConvertEffortIndicator t Rational)
    rrEffortFromRational :: t -> (RoundedRealEffortIndicator t) -> (ConvertEffortIndicator Rational t)
    rrEffortAbs :: t -> (RoundedRealEffortIndicator t) -> (AbsEffortIndicator t)
    rrEffortField :: t -> (RoundedRealEffortIndicator t) -> (FieldOpsEffortIndicator t)
    rrEffortIntMixedField :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator t Int)
    rrEffortIntegerMixedField :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator t Integer)
    rrEffortDoubleMixedField :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator t Double)
    rrEffortRationalMixedField :: t -> (RoundedRealEffortIndicator t) -> (MixedFieldOpsEffortIndicator t Rational)
