{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder.Comparison
    Description :  pointwise up/down comparison
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Pointwise up/down comparison of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder.Comparison
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Config
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison
import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

--import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

instance
    (Ord var, Show var, 
     Show cf, 
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     RefOrd.IntervalLike cf) 
    => 
    NumOrd.PartialComparison (IntPoly var cf) 
    where
    type NumOrd.PartialCompareEffortIndicator (IntPoly var cf) =
        (Int1To1000, ArithInOut.RoundedRealEffortIndicator cf) 
    pCompareDefaultEffort (IntPoly cfg terms) = 
        (Int1To1000 $ 4 * varsN, ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg)
        where
        varsN = length vars
        vars = ipolycfg_vars cfg
    pCompareEff eff p1 p2 =
        case partialInfo2PartialOrdering $ NumOrd.pCompareInFullEff eff p1 p2 of
            [rel] -> Just rel
            _ -> Nothing
    pCompareInFullEff (Int1To1000 n, effDom) p1 = 
        pCompareFunFromRingOps (n, effDom, effCompDom, effDom) p1 
        where
        effCompDom = ArithInOut.rrEffortNumComp sampleDom effDom
        sampleDom = getSampleDomValue p1
        
instance
    (Ord var,
     NumOrd.HasGreatest cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf
    )
    =>
    NumOrd.HasGreatest (IntPoly var cf)
    where
    greatest sampleF = 
        newConstFnFromSample sampleF (NumOrd.greatest sampleDom)
        where
        sampleDom = getSampleDomValue sampleF
        
instance
    (Ord var,
     NumOrd.HasLeast cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf
    )
    =>
    NumOrd.HasLeast (IntPoly var cf)
    where
    least sampleF = 
        newConstFnFromSample sampleF (NumOrd.least sampleDom)
        where
        sampleDom = getSampleDomValue sampleF
        
instance
    (Ord var,
     NumOrd.HasExtrema cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf
    )
    =>
    NumOrd.HasExtrema (IntPoly var cf)
        

        
--instance
--    (
--    )
--    =>
--    ArithUpDn.Convertible (IntPoly var cf) cf
--    where
--    type ArithUpDn.ConvertEffortIndicator (IntPoly var cf) cf = 
--        ArithInOut.RoundedRealEffortIndicator cf
--    convertDnEff = 
        