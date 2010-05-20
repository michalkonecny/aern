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
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

instance 
    (ArithInOut.RoundedMixedAdd Integer e, 
     ArithInOut.RoundedMultiply e, 
     ArithInOut.RoundedMixedDivide Integer e,
     ArithUpDn.Convertible e Integer,
     ArithUpDn.Convertible Double e,
     HasZero e,
     HasInfinities e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e) => 
    (ArithInOut.RoundedExponentiation (Interval e))
    where
    type ArithInOut.ExpEffortIndicator (Interval e) = 
        ((ArithInOut.MixedAddEffortIndicator Integer e, 
          ArithInOut.MultEffortIndicator e,
          ArithInOut.MixedDivEffortIndicator Integer e)
        ,
         Int1To100
        ,
         (NumOrd.MinmaxEffortIndicator e, 
          ArithUpDn.ConvertEffortIndicator e Integer,
          ArithUpDn.ConvertEffortIndicator Double e)
        )
    expDefaultEffortIndicator (Interval l h) = 
        ((ArithInOut.mixedAddDefaultEffort sampleI l, 
          ArithInOut.multDefaultEffort l,
          ArithInOut.mixedDivDefaultEffort sampleI l)
        ,
         Int1To100 3
        , 
         (NumOrd.minmaxDefaultEffort l, 
          ArithUpDn.convertDefaultEffort l sampleI,
          ArithUpDn.convertDefaultEffort sampleD l)
        )
         where
         sampleI = 1 :: Integer
         sampleD = 1 :: Double
    expOutEff effort@(effortOps,  effortTaylor, effortMeetConv@(effortMeet, _, _)) (Interval l h) =
        RefOrd.meetOutEff effortMeet
            (expOutThinArg effortOps effortMeetConv effortTaylor lI) 
            (expOutThinArg effortOps effortMeetConv effortTaylor hI)
        where
        lI = Interval l l
        hI = Interval h h
--    expInEff effort@(effortOps,  effortTaylor, effortMeet) (Interval l h) =
--        RefOrd.meetInEff effortMeet
--            (flipConsistency $ expOutThinArg effortOps  effortTaylor lI) 
--            (flipConsistency $ expOutThinArg effortOps effortTaylor hI)
--        where
--        lI = Interval l l
--        hI = Interval h h
        