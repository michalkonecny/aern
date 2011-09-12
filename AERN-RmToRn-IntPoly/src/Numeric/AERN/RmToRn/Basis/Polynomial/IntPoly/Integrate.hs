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
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps

import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

integratePolyMainVar ::
    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    IntPoly var cf {- initial value at point 0 -} ->
    IntPoly var cf {- polynomial to integrate in its main variable -} ->
    IntPoly var cf
integratePolyMainVar eff z initPoly (IntPoly cfg poly) = 
    initPoly <+> (IntPoly cfg $ ip poly) 
    where
    (<+>) = ArithInOut.addOutEff effAdd
    effDiv = ArithInOut.mxfldEffortDiv sample (1::Int) $ ArithInOut.rrEffortIntMixedField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    sample = ipolycfg_sample_cf cfg
    ip (IntPolyG x coeffs) = (IntPolyG x $ coeffsFractions ++ [z])
        where
        coeffsFractions = 
            zipWith (</>|) coeffs [degreePlusOne,degreePlusOne-1..]
            where
            (</>|) = ArithInOut.mixedDivOutEff effDiv
        degreePlusOne = length coeffs
    ip p@(IntPolyV x polys@(polyHighest:_)) =
--    unsafePrint
--    (
--        "integratePoly:"
--        ++ "\n initPoly = " ++ showPoly initPoly
--        ++ "\n p = " ++ showPoly p
--        ++ "\n result = " ++ showPoly result
--    )
        result
        where
        result 
            = 
            IntPolyV x $ polysFractions ++ [intpoly_terms $ newConstFn cfgR undefined z] 
        cfgR = cfg { ipolycfg_vars = tail $ ipolycfg_vars cfg }
        polysFractions = 
            map intpoly_terms $ zipWith (</>|) intpolys [degreePlusOne,degreePlusOne-1..]
            where
            intpolys = map (IntPoly cfg) polys
            (</>|) = ArithInOut.mixedDivOutEff effDiv
        degreePlusOne = length polys
