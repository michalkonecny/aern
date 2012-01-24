{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Integrate
    Description :  integration of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Integration of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Integrate
    (
        integratePolyMainVar
    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Substitution

import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

--import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import qualified Data.IntMap as IntMap

integratePolyMainVar ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf,
     Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    IntPoly var cf {- initial value at point domLE -} ->
    IntPoly var cf {- polynomial to integrate in its main variable -} ->
    IntPoly var cf
integratePolyMainVar eff z initPoly p@(IntPoly cfg poly) =
--    let ?addInOutEffort = effAdd in
    initPoly <+> (IntPoly cfg $ ip poly)
--    (initPoly <+> (neg projectionAtDomLE)) <+> (IntPoly cfg $ ip poly) 
    where
    (<+>) = ArithInOut.addOutEff effAdd
    effDiv = ArithInOut.mxfldEffortDiv sample (1::Int) $ ArithInOut.rrEffortIntMixedField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    sample = ipolycfg_sample_cf cfg
    
--    projectionAtDomLE =
--        substPolyMainVar eff z domLEPoly p
--    domLEPoly =
--        newConstFnFromSample p domLE
--    (domLE, _) = RefOrd.getEndpointsOutWithDefaultEffort $ head doms
--    doms = ipolycfg_doms cfg
    
    ip p@(IntPolyV x polys) =
--    unsafePrint
--    (
--        "integratePoly:"
--        ++ "\n initPoly = " ++ showPoly initPoly
--        ++ "\n p = " ++ showPoly p
--        ++ "\n result = " ++ showPoly result
--    )
        result
        where
        result = IntPolyV x $ polysFractions 
        cfgR = cfgRemVar cfg
        polysFractions =
            IntMap.fromDistinctAscList $
            map integrateTerm
            $ IntMap.toAscList polys
            where
            integrateTerm (n,p) =
                (n+1, intpoly_terms $ (IntPoly cfgR p) </>| (n+1))
            (</>|) = ArithInOut.mixedDivOutEff effDiv
