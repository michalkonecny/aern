{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps
    Description :  elementary in-place operations using generic direct implementation
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary in-place operations using generic implementation directly from
    field operations.
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps
()
where

import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps ()

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation

--import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps.Sqrt

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

--import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Measures

import Numeric.AERN.Basics.Interval
--import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

import Numeric.AERN.Basics.Mutable

instance
    (CanBeMutable e, 
     ArithInOut.RoundedRealInPlace (Interval e),
     -- MK has no idea why the following three are not automatically deduced from the above...
     ArithUpDn.RoundedReal e,
     ArithInOut.RoundedAddEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     EffortIndicator (ArithInOut.FieldOpsEffortIndicator (Distance e))
    ) 
    => 
    (ArithInOut.RoundedExponentiationInPlace (Interval e))
    where
    expOutInPlaceEff 
        eff
        (MInterval resL resR)
        (MInterval lM rM) =
            do
            forgetMeL <- cloneMutable lM
            forgetMeR <- cloneMutable lM  
            expOutThinArgInPlace eff
                (MInterval resL forgetMeR)
                (MInterval lM lM)
            expOutThinArgInPlace eff
                (MInterval forgetMeL resR)
                (MInterval rM rM)
    expInInPlaceEff 
        eff
        (MInterval resL resR)
        (MInterval lM rM) =
            do
            forgetMeL <- cloneMutable lM
            forgetMeR <- cloneMutable lM  
            expOutThinArgInPlace eff 
                (MInterval forgetMeL resL)
                (MInterval lM lM)
            expOutThinArgInPlace eff
                (MInterval resR forgetMeR)
                (MInterval rM rM)

instance
    (CanBeMutable e, Show e,
     ArithUpDn.RoundedFieldInPlace e,
     ArithUpDn.RoundedMixedFieldInPlace e Int,
     ArithUpDn.RoundedReal e, 
     NumOrd.RoundedLatticeInPlace e) 
    => 
    (ArithInOut.RoundedSquareRootInPlace (Interval e))
    where
    sqrtOutInPlaceEff
        = pureToMutable1Eff ArithInOut.sqrtOutEff

-- the following is a proper in-place version - but it currently fails the test        
        
--        ((effortField, effortMixedField),
--         (Int1To10 effortNewton),
--         ((effortMeet, effortComp), effortConv)) 
--        (MInterval resL resR)
--        (MInterval lM rM) =
--            do
--            forgetMeL <- cloneMutable lM
--            forgetMeR <- cloneMutable lM  
--            sqrtOutThinArgInPlace 
--                effortField effortMixedField 
--                effortMeet effortComp effortConv 
--                (MInterval resL forgetMeR)
--                effortNewton 
--                lM
--            sqrtOutThinArgInPlace
--                effortField effortMixedField
--                effortMeet effortComp effortConv 
--                (MInterval forgetMeL resR)
--                effortNewton 
--                rM
    sqrtInInPlaceEff 
        = pureToMutable1Eff ArithInOut.sqrtInEff
--        ((effortField, effortMixedField),
--         (Int1To10 effortNewton),
--         ((effortMeet, effortComp), effortConv)) 
--        (MInterval resL resR)
--        (MInterval lM rM) =
--            do
--            forgetMeL <- cloneMutable lM
--            forgetMeR <- cloneMutable lM  
--            sqrtOutThinArgInPlace 
--                effortField effortMixedField 
--                effortMeet effortComp effortConv 
--                (MInterval forgetMeL resL)
--                effortNewton 
--                lM
--            sqrtOutThinArgInPlace
--                effortField effortMixedField
--                effortMeet effortComp effortConv 
--                (MInterval resR forgetMeR)
--                effortNewton 
--                rM
            
            
