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

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Exception

instance (HasLegalValues e) => HasLegalValues (Interval e) where
    maybeGetProblem (Interval l r) = 
        case (maybeGetProblem l, maybeGetProblem r) of
            (Nothing, Nothing) -> Nothing
            (Just problemDescription, _) -> 
                Just $ "in the left endpoint of an interval: " ++ problemDescription
            (_, Just problemDescription) ->  
                Just $ "in the right endpoint of an interval: " ++ problemDescription

instance
    (ArithUpDn.RoundedReal e, NumOrd.HasExtrema e, 
     ArithInOut.RoundedAdd (Distance e), 
     Neg (Distance e),
     RefOrd.OuterRoundedLattice (Distance e)
    ) => 
    ArithInOut.RoundedReal (Interval e) 
    where
    type ArithInOut.RoundedRealEffortIndicator (Interval e) = 
        (ArithUpDn.RoundedRealEffortIndicator e, 
         (ArithInOut.AddEffortIndicator (Distance e),
          RefOrd.JoinMeetOutEffortIndicator (Distance e)
         ))
    roundedRealDefaultEffort (Interval l r) = 
        (ArithUpDn.roundedRealDefaultEffort l, 
        (ArithInOut.addDefaultEffort sampleDist, RefOrd.joinmeetOutDefaultEffort sampleDist))
        where
        sampleDist =  distanceBetweenEff (distanceDefaultEffort l) l l
    rrEffortNumComp (Interval l r) (effR,_) = ArithUpDn.rrEffortComp l effR
    rrEffortMinmaxIn (Interval l r) (effR,_) = ArithUpDn.rrEffortMinmax l effR
    rrEffortMinmaxOut (Interval l r) (effR,_) = ArithUpDn.rrEffortMinmax l effR
    rrEffortRefComp (Interval l r) (effR,_) = ArithUpDn.rrEffortComp l effR
    rrEffortJoinMeetOut (Interval l r) (effR,_) = ArithUpDn.rrEffortMinmax l effR
    rrEffortJoinMeetIn (Interval l r) (effR,_) = ArithUpDn.rrEffortMinmax l effR
    rrEffortDistance (Interval l r) (effR,(effDistAdd, _)) 
        = (ArithUpDn.rrEffortDistance l effR, effDistAdd) 
    rrEffortImprecision (Interval l r) (effR,(effDistAdd, effDistJoin))
        = (ArithUpDn.rrEffortDistance l effR, effDistJoin, ArithUpDn.rrEffortComp l effR)
    rrEffortToInt (Interval l r) (effR,_) = ArithUpDn.rrEffortToInt l effR
    rrEffortFromInt (Interval l r) (effR,_) = ArithUpDn.rrEffortFromInt l effR
    rrEffortToInteger (Interval l r) (effR,_) = ArithUpDn.rrEffortToInteger l effR
    rrEffortFromInteger (Interval l r) (effR,_) = ArithUpDn.rrEffortFromInteger l effR
    rrEffortToDouble (Interval l r) (effR,_) = ArithUpDn.rrEffortToDouble l effR
    rrEffortFromDouble (Interval l r) (effR,_) = ArithUpDn.rrEffortFromDouble l effR
    rrEffortToRational (Interval l r) (effR,_) = ArithUpDn.rrEffortToRational l effR
    rrEffortFromRational (Interval l r) (effR,_) = ArithUpDn.rrEffortFromRational l effR
    rrEffortAbs (Interval l r) (effR,_) =
         (ArithUpDn.rrEffortComp l effR, 
         ArithUpDn.rrEffortMinmax l effR)
    rrEffortField (Interval l r) (effR,_) = 
        (ArithUpDn.rrEffortField l effR,
         ArithUpDn.rrEffortComp l effR, 
         ArithUpDn.rrEffortMinmax l effR)
    rrEffortIntMixedField (Interval l r) (effR,_) =
        (ArithUpDn.rrEffortIntMixedField l effR,
         (ArithUpDn.rrEffortComp l effR,
          ArithUpDn.rrEffortMinmax l effR,
          ()
         ),
         ((ArithUpDn.rrEffortComp l effR,
           ArithUpDn.rrEffortMinmax l effR,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l effR),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l effR)
           )
          ),
          ArithUpDn.rrEffortFromInt l effR
         )
        )
    rrEffortIntegerMixedField (Interval l r) (effR,_) =
        (ArithUpDn.rrEffortIntegerMixedField l effR,
         (ArithUpDn.rrEffortComp l effR,
          ArithUpDn.rrEffortMinmax l effR,
          ()
         ),
         ((ArithUpDn.rrEffortComp l effR,
           ArithUpDn.rrEffortMinmax l effR,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l effR),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l effR)
           )
          ),
          ArithUpDn.rrEffortFromInteger l effR
         )
        )
    rrEffortDoubleMixedField (Interval l r) (effR,_) =
        (ArithUpDn.rrEffortDoubleMixedField l effR,
         (ArithUpDn.rrEffortComp l effR,
          ArithUpDn.rrEffortMinmax l effR,
          ()
         ),
         ((ArithUpDn.rrEffortComp l effR,
           ArithUpDn.rrEffortMinmax l effR,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l effR),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l effR)
           )
          ),
          ArithUpDn.rrEffortFromDouble l effR
         )
        )
    rrEffortRationalMixedField (Interval l r) (effR,_) =
        (ArithUpDn.rrEffortRationalMixedField l effR,
         (ArithUpDn.rrEffortComp l effR,
          ArithUpDn.rrEffortMinmax l effR,
          ()
         ),
         ((ArithUpDn.rrEffortComp l effR,
           ArithUpDn.rrEffortMinmax l effR,
           ((ArithUpDn.fldEffortMult l $ ArithUpDn.rrEffortField l effR),
            (ArithUpDn.fldEffortDiv l $ ArithUpDn.rrEffortField l effR)
           )
          ),
          ArithUpDn.rrEffortFromRational l effR
         )
        )

