{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval
    Description :  instances of arithmetic classes for Intervals  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Instances of arithmetic classes for Intervals.
-}

module Numeric.AERN.RealArithmetic.Interval
()
where

import Numeric.AERN.RealArithmetic.Interval.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Measures
import Numeric.AERN.RealArithmetic.Interval.Conversion
import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps
import Numeric.AERN.RealArithmetic.Interval.SpecialConst
import Numeric.AERN.RealArithmetic.Interval.Floating

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Exception

instance (HasLegalValues e) => HasLegalValues (Interval e) where
    isLegal (Interval l r) = isLegal l && isLegal r 

instance
    (ArithUpDn.RoundedReal e, NumOrd.HasExtrema e) => 
    ArithInOut.RoundedReal (Interval e) 
    where
    type ArithInOut.RoundedRealEffortIndicator (Interval e) = 
        ArithUpDn.RoundedRealEffortIndicator e
    roundedRealDefaultEffort (Interval l r) = ArithUpDn.roundedRealDefaultEffort l 
    rrEffortNumComp (Interval l r) eff = ArithUpDn.rrEffortComp l eff
    rrEffortMinmaxIn (Interval l r) eff = ArithUpDn.rrEffortMinmax l eff
    rrEffortMinmaxOut (Interval l r) eff = ArithUpDn.rrEffortMinmax l eff
    rrEffortRefComp (Interval l r) eff = ArithUpDn.rrEffortComp l eff
    rrEffortJoinMeetOut (Interval l r) eff = ArithUpDn.rrEffortMinmax l eff
    rrEffortJoinMeetIn (Interval l r) eff = ArithUpDn.rrEffortMinmax l eff
    rrEffortToInt (Interval l r) eff = ArithUpDn.rrEffortToInt l eff
    rrEffortFromInt (Interval l r) eff = ArithUpDn.rrEffortFromInt l eff
    rrEffortToInteger (Interval l r) eff = ArithUpDn.rrEffortToInteger l eff
    rrEffortFromInteger (Interval l r) eff = ArithUpDn.rrEffortFromInteger l eff
    rrEffortToDouble (Interval l r) eff = ArithUpDn.rrEffortToDouble l eff
    rrEffortFromDouble (Interval l r) eff = ArithUpDn.rrEffortFromDouble l eff
    rrEffortToRational (Interval l r) eff = ArithUpDn.rrEffortToRational l eff
    rrEffortFromRational (Interval l r) eff = ArithUpDn.rrEffortFromRational l eff
    rrEffortAbs (Interval l r) eff =
         (ArithUpDn.rrEffortComp l eff, 
         ArithUpDn.rrEffortMinmax l eff)
    rrEffortField (Interval l r) eff = 
        (ArithUpDn.rrEffortField l eff,
         ArithUpDn.rrEffortComp l eff, 
         ArithUpDn.rrEffortMinmax l eff)
    rrEffortIntMixedField (Interval l r) eff =
        (ArithUpDn.rrEffortIntMixedField l eff,
         (ArithUpDn.rrEffortComp l eff,
          ArithUpDn.rrEffortMinmax l eff,
          ()
         ),
         ((ArithUpDn.rrEffortComp l eff,
           ArithUpDn.rrEffortMinmax l eff,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l eff),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l eff)
           )
          ),
          ArithUpDn.rrEffortFromInt l eff
         )
        )
    rrEffortIntegerMixedField (Interval l r) eff =
        (ArithUpDn.rrEffortIntegerMixedField l eff,
         (ArithUpDn.rrEffortComp l eff,
          ArithUpDn.rrEffortMinmax l eff,
          ()
         ),
         ((ArithUpDn.rrEffortComp l eff,
           ArithUpDn.rrEffortMinmax l eff,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l eff),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l eff)
           )
          ),
          ArithUpDn.rrEffortFromInteger l eff
         )
        )
    rrEffortDoubleMixedField (Interval l r) eff =
        (ArithUpDn.rrEffortDoubleMixedField l eff,
         (ArithUpDn.rrEffortComp l eff,
          ArithUpDn.rrEffortMinmax l eff,
          ()
         ),
         ((ArithUpDn.rrEffortComp l eff,
           ArithUpDn.rrEffortMinmax l eff,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l eff),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l eff)
           )
          ),
          ArithUpDn.rrEffortFromDouble l eff
         )
        )
    rrEffortRationalMixedField (Interval l r) eff =
        (ArithUpDn.rrEffortRationalMixedField l eff,
         (ArithUpDn.rrEffortComp l eff,
          ArithUpDn.rrEffortMinmax l eff,
          ()
         ),
         ((ArithUpDn.rrEffortComp l eff,
           ArithUpDn.rrEffortMinmax l eff,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l eff),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l eff)
           )
          ),
          ArithUpDn.rrEffortFromRational l eff
         )
        )

