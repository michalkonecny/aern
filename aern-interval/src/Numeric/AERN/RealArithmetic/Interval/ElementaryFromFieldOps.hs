{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
(intervalExpDefaultEffortWithIters, intervalExpOutIters, intervalExpInIters)
where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation

import Numeric.AERN.RealArithmetic.NumericOrderRounding.ElementaryFromFieldOps.Sqrt

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
        (
                ExpEffortIndicator,
                SqrtEffortIndicator
        )

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

--import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Measures

import Numeric.AERN.Basics.Interval
--import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals

instance
    (ArithInOut.RoundedReal (Interval e),
     -- MK has no idea why the following three are not automatically deduced from the above...
     ArithUpDn.RoundedReal e,
     ArithInOut.RoundedAddEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     EffortIndicator
                        (ArithInOut.FieldOpsEffortIndicator (Distance e))
     )
    => 
    (ArithInOut.RoundedExponentiationEffort (Interval e))
    where
    type ExpEffortIndicator (Interval e) = 
        (ArithInOut.RoundedRealEffortIndicator (Interval e), Int1To10)
    expDefaultEffort i =
        (ArithInOut.roundedRealDefaultEffort i, Int1To10 10)

intervalExpDefaultEffortWithIters ::
    (ArithInOut.RoundedReal (Interval e))
    => 
    (Interval e) -> 
    Int -> 
    ArithInOut.ExpEffortIndicator (Interval e)
intervalExpDefaultEffortWithIters  i@(Interval _ _) n =
    (ArithInOut.roundedRealDefaultEffort i, Int1To10 n)


instance
    (ArithInOut.RoundedReal (Interval e), 
     -- MK has no idea why the following three are not automatically deduced from the above...
     ArithUpDn.RoundedReal e,
     ShowInternals e,
     ArithInOut.RoundedAddEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     EffortIndicator (ArithInOut.FieldOpsEffortIndicator (Distance e)),
     --
     NumOrd.HasExtrema e)
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

intervalExpOutIters, intervalExpInIters ::
    (ArithInOut.RoundedReal (Interval e), 
     -- MK has no idea why the following three are not automatically deduced from the above...
     ArithUpDn.RoundedReal e,
     ShowInternals e,
     ArithInOut.RoundedAddEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     EffortIndicator (ArithInOut.FieldOpsEffortIndicator (Distance e)),
     --
     NumOrd.HasExtrema e)
    => 
    Int -> (Interval e) -> (Interval e)
intervalExpOutIters n i = ArithInOut.expOutEff (intervalExpDefaultEffortWithIters i n) i
intervalExpInIters n i = ArithInOut.expInEff (intervalExpDefaultEffortWithIters i n) i

instance 
    (ArithUpDn.RoundedReal e) 
    => 
    (ArithInOut.RoundedSquareRootEffort (Interval e))
    where
    type SqrtEffortIndicator (Interval e) = SqrtThinEffortIndicator e 
    sqrtDefaultEffort (Interval l _) =
        sqrtThinDefaultEffort l 10 

instance 
    (ArithUpDn.RoundedReal e, Show e) 
    => 
    (ArithInOut.RoundedSquareRoot (Interval e))
    where
    sqrtOutEff eff (Interval l r) =
        case NumOrd.pEqualEff effComp l r of
            Just True -> Interval sqrtLL sqrtLR
            _ -> Interval sqrtLL sqrtRR
        where
        (sqrtLL, sqrtLR) = sqrtOutThinArg eff l 
        (_, sqrtRR) = sqrtOutThinArg eff r
        effComp = 
            ArithUpDn.rrEffortComp l $ sqrteff_arith eff 
    sqrtInEff eff (Interval l r) =
        case NumOrd.pEqualEff effComp l r of
            Just True -> Interval sqrtLR sqrtLL -- invert
            _ -> Interval sqrtLR sqrtRL
        where
        (sqrtLL, sqrtLR) = sqrtOutThinArg eff l 
        (sqrtRL, _) = sqrtOutThinArg eff r
        effComp = 
            ArithUpDn.rrEffortComp l $ sqrteff_arith eff 
                
