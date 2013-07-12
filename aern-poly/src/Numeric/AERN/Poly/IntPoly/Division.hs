{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Division
    Description :  out-rounded polynomial division
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Out-rounded polynomial division.  Currently very inaccurate.
-}

module Numeric.AERN.Poly.IntPoly.Division
    (
    )
where
    
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.Conversion ()
import Numeric.AERN.Poly.IntPoly.Multiplication ()
import Numeric.AERN.Poly.IntPoly.Evaluation ()

import Numeric.AERN.RmToRn

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.SizeLimits
import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug


instance
    (Ord var, Show var, Show cf, Show (SizeLimits cf),
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf, 
     RefOrd.IntervalLike cf)
    =>
    ArithInOut.RoundedDivideEffort (IntPoly var cf) 
    where
    type DivEffortIndicator (IntPoly var cf) = 
        (EvaluationEffortIndicator (IntPoly var cf),
         ArithInOut.MultEffortIndicator (IntPoly var cf))
    divDefaultEffort p = 
        (evaluationDefaultEffort p,
         ArithInOut.multDefaultEffort p)  

instance
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Ord var, Show var, Show cf) 
    =>
    ArithInOut.RoundedDivide (IntPoly var cf) 
    where
    divOutEff (effEval, effMulF) p1 = 
        divOutEffDummyViaRange effEval effMulF p1
    divInEff = 
        error "aern-poly: IntPoly does not support inwards-rounded multiplication" 

divOutEffDummyViaRange ::
    (CanEvaluate f,
     HasConstFns f,
     ArithInOut.RoundedMultiply f)
    =>
    (EvaluationEffortIndicator f) ->
    (ArithInOut.MultEffortIndicator f) ->
    f -> f -> f
divOutEffDummyViaRange effEval effMulF f1 f2 =
    ArithInOut.multOutEff effMulF f1 $ newConstFnFromSample f1 f2Range
    where
    f2Range = evalAtPointOutEff effEval (getDomainBox f2) f2 

