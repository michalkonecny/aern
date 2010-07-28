{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ElementaryDirect
    Description :  elementary operations using generic direct implementation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary operations using generic direct implementation.
-}

module Numeric.AERN.RealArithmetic.Interval.ElementaryDirect where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Implementation.Elementary

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

instance 
    (ArithInOut.RoundedMixedField Int (Interval e),
     ArithInOut.RoundedField (Interval e), 
     ArithUpDn.Convertible (Interval e) Int,
     ArithInOut.Convertible Double (Interval e),
     HasZero e, HasOne e, 
     HasInfinities e,
     NumOrd.PartialComparison e,
     RefOrd.OuterRoundedLattice (Interval e)) => 
    (ArithInOut.RoundedExponentiation (Interval e))
    where
    type ArithInOut.ExpEffortIndicator (Interval e) = 
        ((ArithInOut.FieldOpsEffortIndicator (Interval e),
          ArithInOut.MixedFieldOpsEffortIndicator Int (Interval e))
        ,
         Int1To10
        ,
         ((RefOrd.JoinMeetOutEffortIndicator (Interval e),
           NumOrd.PartialCompareEffortIndicator e), 
          (ArithUpDn.ConvertEffortIndicator (Interval e) Int,
           ArithInOut.ConvertEffortIndicator Double (Interval e)))
        )
    expDefaultEffortIndicator i@(Interval l h) = 
        ((ArithInOut.fieldOpsDefaultEffort i, 
          ArithInOut.mixedFieldOpsDefaultEffort sampleI i)
        ,
         Int1To10 10
        , 
         ((RefOrd.joinmeetOutDefaultEffort i,
           NumOrd.pCompareDefaultEffort l), 
          (ArithUpDn.convertDefaultEffort i sampleI,
           ArithInOut.convertDefaultEffort sampleD i))
        )
        where
        sampleI = 1 :: Int
        sampleD = 1 :: Double
    expOutEff 
            ((effortField, effortMixedField),
             (Int1To10 effortTaylor),
             ((effortMeet, effortComp), effortConv)) 
            (Interval l h) =
                    Interval (fst $ getEndpoints expL) (snd $ getEndpoints expH)
        where
        expL = 
            expOutThinArg 
                effortField effortMixedField 
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                lI
        expH =
            expOutThinArg 
                effortField effortMixedField
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                hI
        lI = Interval l l
        hI = Interval h h
    expInEff 
            ((effortField, effortMixedField),
             (Int1To10 effortTaylor),
             ((effortMeet, effortComp), effortConv)) 
            (Interval l h) =
                    Interval (snd $ getEndpoints expL) (fst $ getEndpoints expH)
        where
        expL = 
            expOutThinArg 
                effortField effortMixedField 
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                lI
        expH =
            expOutThinArg 
                effortField effortMixedField
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                hI
        lI = Interval l l
        hI = Interval h h
        