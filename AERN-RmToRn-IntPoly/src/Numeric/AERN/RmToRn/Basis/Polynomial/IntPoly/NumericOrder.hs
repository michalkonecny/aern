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
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Auxiliary

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug

--instance
--    (ArithInOut.RoundedReal cf, Ord var, Show var, Show cf) => 
--    NumOrd.PartialComparison (IntPoly var cf) 
--    where
--    type NumOrd.PartialCompareEffortIndicator (IntPoly var cf) = 
--        ArithInOut.RoundedRealEffortIndicator cf 
--    pCompareDefaultEffort (IntPoly cfg terms) = 
--        ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg    
--    pCompareEff effDom p1@(IntPoly cfg _) p2 =
--        NumOrd.pCompareEff effCompDom diffRange (zero diffRange)
--        where
--        diffRange = evalOtherType (evalOpsOut diff sampleDom) dom diff
--        diff = ArithInOut.subtrOutEff effAdd p1 p2
--        dom = getDomainBox diff
--        sampleDom = ipolycfg_sample_cf cfg
--        effCompDom = ArithInOut.rrEffortNumComp sampleDom effDom
--        effAdd = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
--        
