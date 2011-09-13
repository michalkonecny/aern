{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps
    Description :  elementary operations using generic direct implementation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary operations using generic direct implementation.
-}

module Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps 
(expDefaultEffortWithIters, expOutIters, expInIters,
 sqrtDefaultEffortWithIters, sqrtOutIters, sqrtInIters)
where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation

import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps.Sqrt

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
    (ArithUpDn.RoundedReal e, NumOrd.HasExtrema e)
    => 
    (ArithInOut.RoundedExponentiationEffort (Interval e))
    where
    type ArithInOut.ExpEffortIndicator (Interval e) = 
        (ArithUpDn.RoundedReal e, Int1To10)
    expDefaultEffort i@(Interval l r) =
        (ArithUpDn.roundedRealDefaultEffort l, Int1To10 10)

expDefaultEffortWithIters ::
    (ArithUpDn.RoundedReal e, NumOrd.HasExtrema e)
    => 
    (Interval e) -> 
    Int -> 
    ArithInOut.ExpEffortIndicator (Interval e)
expDefaultEffortWithIters  i n =
        (ArithUpDn.roundedRealDefaultEffort l, Int1To10 n)


instance
    (ArithUpDn.RoundedReal e, NumOrd.HasExtrema e)
    => 
    (ArithInOut.RoundedExponentiation (Interval e))
    where
    expOutEff 
            (eff, (Int1To10 effortTaylor)) 
            (Interval l r) 
        = Interval resL resR
        where
        Interval resL _ = 
            expOutThinArg 
                eff 
                effortTaylor 
                (Interval l l)
        Interval _ resR =
            expOutThinArg 
                eff 
                effortTaylor 
                (Interval r r)
    expInEff 
            (eff, (Int1To10 effortTaylor)) 
            (Interval l r) 
        = Interval resL resR
        where
        Interval _ resL = 
            expOutThinArg 
                eff 
                effortTaylor 
                (Interval l l)
        Interval resR _ =
            expOutThinArg 
                eff 
                effortTaylor 
                (Interval r r)

expOutIters, expInIters ::
    (ArithUpDn.RoundedReal e, NumOrd.HasExtrema e)
    => 
    Int -> (Interval e) -> (Interval e)
expOutIters n i = ArithInOut.expOutEff (expDefaultEffortWithIters i n) i
expInIters n i = ArithInOut.expInEff (expDefaultEffortWithIters i n) i

instance 
    (ArithUpDn.RoundedMixedFieldEffort e Int,
     ArithUpDn.RoundedFieldEffort e, 
     ArithUpDn.Convertible e Double,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e) 
    => 
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

    sqrtDefaultEffort i@(Interval l r) = 
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

sqrtDefaultEffortWithIters ::
    (ArithUpDn.RoundedMixedFieldEffort e Int,
     ArithUpDn.RoundedFieldEffort e, 
     ArithUpDn.Convertible e Double,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e) 
    => 
    (Interval e) -> 
    Int -> 
    ArithInOut.SqrtEffortIndicator (Interval e)
sqrtDefaultEffortWithIters i@(Interval l r) n =
        ((ArithUpDn.fieldOpsDefaultEffort l, 
          ArithUpDn.mixedFieldOpsDefaultEffort l sampleI)
        ,
         Int1To10 n
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
     Show e) 
    => 
    (ArithInOut.RoundedSquareRoot (Interval e))
    where
    sqrtOutEff
            ((effortField, effortMixedField),
             (Int1To10 effortNewton),
             ((effortMinmax, effortComp), effortConv))
            (Interval l r) =
                case NumOrd.pEqualEff effortComp l r of
                    Just True -> sqrtL
                    _ -> Interval sqrtLL sqrtRR
                    
        where
        sqrtL@(Interval sqrtLL _) = sqrt l 
        sqrtR@(Interval _ sqrtRR) = sqrt r
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
            (Interval l r) =
                case NumOrd.pEqualEff effortComp l r of
                    Just True -> Interval sqrtLR sqrtLL -- invert
                    _ -> Interval sqrtLR sqrtRL
                    
        where
        (Interval sqrtLL sqrtLR) = sqrt l 
        (Interval sqrtRL sqrtRR) = sqrt r
        sqrt = 
            sqrtOutThinArg 
                effortField
                effortMixedField 
                effortMinmax
                effortComp
                effortConv 
                effortNewton 

sqrtOutIters, sqrtInIters ::
    (ArithUpDn.RoundedMixedField e Int,
     ArithUpDn.RoundedField e, 
     ArithUpDn.Convertible e Double,
     HasZero e, HasOne e, 
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e,
     Show e)
    =>
    Int -> (Interval e) -> (Interval e) 
sqrtOutIters n i = ArithInOut.sqrtOutEff (sqrtDefaultEffortWithIters i n) i
sqrtInIters n i = ArithInOut.sqrtInEff (sqrtDefaultEffortWithIters i n) i
                