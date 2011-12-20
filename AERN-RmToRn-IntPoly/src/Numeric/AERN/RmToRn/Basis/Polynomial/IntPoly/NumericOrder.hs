{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder
    Description :  pointwise up/down comparison
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Pointwise up/down comparison of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder
    (
    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
--import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation

import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

--import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Misc.Debug

instance
    (Ord var, Show var, 
     Show cf, ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf) 
    => 
    NumOrd.PartialComparison (IntPoly var cf) 
    where
    type NumOrd.PartialCompareEffortIndicator (IntPoly var cf) =
        (Int, ArithInOut.RoundedRealEffortIndicator cf) 
    pCompareDefaultEffort (IntPoly cfg terms) = 
        (4 * varsN, ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg)
        where
        varsN = length vars
        vars = ipolycfg_vars cfg
    pCompareEff eff p1 p2 =
        case partialInfo2PartialOrdering $ NumOrd.pCompareInFullEff eff p1 p2 of
            [rel] -> Just rel
            _ -> Nothing 
    pCompareInFullEff (n, effDom) p1 = pCompareFunFromRingOps (n, effAdd, effCompDom, effDom) p1 
        where
        effCompDom = ArithInOut.rrEffortNumComp sampleDom effDom
        effAdd = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
        sampleDom = getSampleDomValue p1
        