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

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation

import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect.Sqrt

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

instance
    (ArithInOut.RoundedMixedField (Interval e) Int,
     ArithInOut.RoundedField (Interval e), 
     ArithUpDn.Convertible (Interval e) Int,
     ArithInOut.Convertible Double (Interval e),
     NumOrd.PartialComparison e,
     RefOrd.OuterRoundedLatticeEffort (Interval e)) => 
    (ArithInOut.RoundedExponentiationEffort (Interval e))
    where
    type ArithInOut.ExpEffortIndicator (Interval e) = 
        ((ArithInOut.FieldOpsEffortIndicator (Interval e),
          ArithInOut.MixedFieldOpsEffortIndicator (Interval e) Int)
        ,
         Int1To10
        ,
         ((RefOrd.JoinMeetOutEffortIndicator (Interval e),
           NumOrd.PartialCompareEffortIndicator e), 
          (ArithUpDn.ConvertEffortIndicator (Interval e) Int,
           ArithInOut.ConvertEffortIndicator Double (Interval e)))
        )
    expDefaultEffort i@(Interval l h) = 
        ((ArithInOut.fieldOpsDefaultEffort i, 
          ArithInOut.mixedFieldOpsDefaultEffort i sampleI)
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

instance
    (ArithInOut.RoundedMixedField (Interval e) Int,
     ArithInOut.RoundedField (Interval e), 
     ArithUpDn.Convertible (Interval e) Int,
     ArithInOut.Convertible Double (Interval e),
     HasZero e, HasOne e, 
     HasInfinities e,
     NumOrd.PartialComparison e,
     RefOrd.OuterRoundedLattice (Interval e)) => 
    (ArithInOut.RoundedExponentiation (Interval e))
    where
    expOutEff 
            ((effortField, effortMixedField),
             (Int1To10 effortTaylor),
             ((effortMeet, effortComp), effortConv)) 
            (Interval l h) = Interval resL resH
        where
        Interval resL _ = 
            expOutThinArg 
                effortField effortMixedField 
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                (Interval l l)
        Interval _ resH =
            expOutThinArg 
                effortField effortMixedField
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                (Interval h h)
    expInEff 
            ((effortField, effortMixedField),
             (Int1To10 effortTaylor),
             ((effortMeet, effortComp), effortConv)) 
            (Interval l h) = Interval resL resH
        where
        Interval _ resL = 
            expOutThinArg 
                effortField effortMixedField 
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                (Interval l l)
        Interval resH _ =
            expOutThinArg 
                effortField effortMixedField
                effortMeet effortComp effortComp effortConv 
                effortTaylor 
                (Interval h h)

instance 
    (ArithUpDn.RoundedMixedFieldEffort e Int,
     ArithUpDn.RoundedFieldEffort e, 
     ArithUpDn.Convertible e Double,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e) => 
    (ArithInOut.RoundedSquareRootEffort (Interval e))
    where
    type ArithInOut.SqrtEffortIndicator (Interval e) = 
        ((ArithUpDn.FieldOpsEffortIndicator e,
          ArithUpDn.MixedFieldOpsEffortIndicator e Int)
        ,
         Int1To10
        ,
         ((NumOrd.MinmaxEffortIndicator e, NumOrd.PartialCompareEffortIndicator e),
          ArithUpDn.ConvertEffortIndicator e Double)
        )

    sqrtDefaultEffort i@(Interval l h) = 
        ((ArithUpDn.fieldOpsDefaultEffort l, 
          ArithUpDn.mixedFieldOpsDefaultEffort l sampleI)
        ,
         Int1To10 10
        , 
         ((NumOrd.minmaxDefaultEffort l, NumOrd.pCompareDefaultEffort l), 
          ArithUpDn.convertDefaultEffort l sampleD)
        )
        where
        sampleI = 1 :: Int
        sampleD = 1 :: Double

instance 
    (ArithUpDn.RoundedMixedField e Int,
     ArithUpDn.RoundedField e, 
     ArithUpDn.Convertible e Double,
     HasZero e, HasOne e, 
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e,
     Show e) => 
    (ArithInOut.RoundedSquareRoot (Interval e))
    where
    sqrtOutEff
            ((effortField, effortMixedField),
             (Int1To10 effortNewton),
             ((effortMinmax, effortComp), effortConv))
            (Interval l h) =
                case NumOrd.pEqualEff effortComp l h of
                    Just True -> sqrtL
                    _ -> Interval sqrtLL sqrtHH
                    
        where
        sqrtL@(Interval sqrtLL _) = sqrt l 
        sqrtH@(Interval _ sqrtHH) = sqrt h
        sqrt = 
            sqrtOutThinArg 
                effortField
                effortMixedField 
                effortMinmax
                effortComp
                effortConv
                effortNewton 
    sqrtInEff
            ((effortField, effortMixedField),
             (Int1To10 effortNewton),
             ((effortMinmax, effortComp), effortConv))
            (Interval l h) =
                case NumOrd.pEqualEff effortComp l h of
                    Just True -> Interval sqrtLH sqrtLL -- invert
                    _ -> Interval sqrtLH sqrtHL
                    
        where
        (Interval sqrtLL sqrtLH) = sqrt l 
        (Interval sqrtHL sqrtHH) = sqrt h
        sqrt = 
            sqrtOutThinArg 
                effortField
                effortMixedField 
                effortMinmax
                effortComp
                effortConv 
                effortNewton 
