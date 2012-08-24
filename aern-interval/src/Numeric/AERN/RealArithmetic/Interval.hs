{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
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

import Numeric.AERN.RealArithmetic.Interval.ExactOps ()
import Numeric.AERN.RealArithmetic.Interval.Measures ()
import Numeric.AERN.RealArithmetic.Interval.Conversion ()
import Numeric.AERN.RealArithmetic.Interval.FieldOps ()
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps ()
import Numeric.AERN.RealArithmetic.Interval.SpecialConst ()
import Numeric.AERN.RealArithmetic.Interval.Floating ()

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
     ArithInOut.RoundedField (Distance e), 
     Neg (Distance e),
     RefOrd.RoundedLattice (Distance e)
    ) => 
    ArithInOut.RoundedReal (Interval e) 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type RoundedRealEffortIndicator (Interval e) = 
        (ArithUpDn.RoundedRealEffortIndicator e, 
         (ArithInOut.FieldOpsEffortIndicator (Distance e),
          NumOrd.PartialCompareEffortIndicator (Distance e),
          RefOrd.JoinMeetEffortIndicator (Distance e)
         ))
#else
    type ArithInOut.RoundedRealEffortIndicator (Interval e) = 
        (ArithUpDn.RoundedRealEffortIndicator e, 
         (ArithInOut.FieldOpsEffortIndicator (Distance e),
          NumOrd.PartialCompareEffortIndicator (Distance e),
          RefOrd.JoinMeetEffortIndicator (Distance e)
         ))
#endif
    roundedRealDefaultEffort (Interval l _) = 
        (ArithUpDn.roundedRealDefaultEffort l, 
            (ArithInOut.fieldOpsDefaultEffort sampleDist,
             NumOrd.pCompareDefaultEffort sampleDist, 
             RefOrd.joinmeetDefaultEffort sampleDist
            )
        )
        where
        sampleDist =  distanceBetweenEff (distanceDefaultEffort l) l l
    rrEffortNumComp (Interval l _) (effR,_) = ArithUpDn.rrEffortComp l effR
    rrEffortMinmaxInOut (Interval l _) (effR,_) = ArithUpDn.rrEffortMinmax l effR
    rrEffortRefComp (Interval l _) (effR,_) = ArithUpDn.rrEffortComp l effR
    rrEffortPartialJoin (Interval l _) (effR,_) = (ArithUpDn.rrEffortMinmax l effR, ArithUpDn.rrEffortComp l effR)
    rrEffortJoinMeet (Interval l _) (effR,_) = ArithUpDn.rrEffortMinmax l effR
    rrEffortDistance (Interval l _) (effR,(effDistFld, _,_)) 
        = (effDist, effDistAdd)
        where
        effDist = ArithUpDn.rrEffortDistance l effR 
        effDistAdd = ArithInOut.fldEffortAdd sampleDist effDistFld
        sampleDist = distanceBetweenEff effDist l l
    rrEffortDistanceComp (Interval _ _) (_, (_, effDistComp, _)) = effDistComp
    rrEffortDistanceField (Interval _ _) (_, (effDistFld, _, _)) = effDistFld
    rrEffortImprecision (Interval l _) (effR,(_, _, effDistJoin))
        = (ArithUpDn.rrEffortDistance l effR, effDistJoin, ArithUpDn.rrEffortComp l effR)
    rrEffortImprecisionComp (Interval _ _) (_, (_, effDistComp, _)) = effDistComp
    rrEffortImprecisionField (Interval _ _) (_, (effDistFld, _, _)) = effDistFld
    rrEffortToInt (Interval l _) (effR,_) = ArithUpDn.rrEffortToInt l effR
    rrEffortFromInt (Interval l _) (effR,_) = ArithUpDn.rrEffortFromInt l effR
    rrEffortToInteger (Interval l _) (effR,_) = ArithUpDn.rrEffortToInteger l effR
    rrEffortFromInteger (Interval l _) (effR,_) = ArithUpDn.rrEffortFromInteger l effR
    rrEffortToDouble (Interval l _) (effR,_) = ArithUpDn.rrEffortToDouble l effR
    rrEffortFromDouble (Interval l _) (effR,_) = ArithUpDn.rrEffortFromDouble l effR
    rrEffortToRational (Interval l _) (effR,_) = ArithUpDn.rrEffortToRational l effR
    rrEffortFromRational (Interval l _) (effR,_) = ArithUpDn.rrEffortFromRational l effR
    rrEffortAbs (Interval l _) (effR,_) =
         (ArithUpDn.rrEffortComp l effR, 
         ArithUpDn.rrEffortMinmax l effR)
    rrEffortField (Interval l _) (effR,_) = 
        (ArithUpDn.rrEffortField l effR,
         ArithUpDn.rrEffortComp l effR, 
         ArithUpDn.rrEffortMinmax l effR)
    rrEffortIntMixedField (Interval l _) (effR,_) =
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
    rrEffortIntegerMixedField (Interval l _) (effR,_) =
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
    rrEffortDoubleMixedField (Interval l _) (effR,_) =
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
    rrEffortRationalMixedField (Interval l _) (effR,_) =
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

