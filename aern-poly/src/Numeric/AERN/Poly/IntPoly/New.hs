{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.New
    Description :  creating constant and single-variable polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Creating constant and single-variable polynomials.
-}

module Numeric.AERN.Poly.IntPoly.New
--    (
--    )
where
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
    
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

{-- Basic function-approximation specific ops --}

instance
    (Ord var, ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    (HasDomainBox (IntPoly var cf))
    where
    type (Domain (IntPoly var cf)) = cf
    type (Var (IntPoly var cf)) = var
    type (VarBox (IntPoly var cf)) = Map.Map var
    getSampleDomValue (IntPoly cfg _) = ipolycfg_sample_cf cfg
    defaultDomSplit _ =
        defaultDomSplitUsingEndpointsDefaultEffort
    getDomainBox (IntPoly cfg _) = Map.fromList $ zip vars doms
        where
        vars = ipolycfg_vars cfg
        doms = zipWith (<+>) domsLE domsLZ
        domsLZ = ipolycfg_domsLZ cfg
        domsLE = ipolycfg_domsLE cfg
    getNSamplesFromDomainBox sampleP@(IntPoly cfg _) dombox n =
        getNSamplesFromDomainBoxUsingEndpointsDefaultEffort sampleDom sampleP dombox n
        where
        sampleDom = ipolycfg_sample_cf cfg
    getSampleFromInsideDomainBox sampleP@(IntPoly cfg _) dombox =
        getSampleFromInsideDomainBoxUsingEndpointsDefaultEffort sampleDom sampleP dombox
        where
        sampleDom = ipolycfg_sample_cf cfg

instance (HasSizeLimits (IntPoly var cf))
    where
    type (SizeLimits (IntPoly var cf)) = IntPolyCfg var cf
    defaultSizeLimits = getSizeLimits 
    getSizeLimits (IntPoly cfg _) = cfg
    changeSizeLimits cfg (IntPoly _ terms) 
        | sameVarDoms = 
            IntPoly cfg termsReduced
        | otherwise =
            error $ "attempted to reassign the domain of a polynomial, which is currently impossible"
        where
        sameVarDoms = True -- TODO
        termsReduced = terms -- TODO
         
instance 
    (Ord var, ArithInOut.RoundedReal cf,
     HasConsistency cf,
     RefOrd.IntervalLike cf) => 
    (HasConstFns (IntPoly var cf))
    where
    newConstFn cfg _ value = 
        IntPoly cfg $ mkConstTerms value $ ipolycfg_vars cfg

mkConstTerms ::
    (HasConsistency cf)
    => 
    cf -> [var] -> IntPolyTerms var cf
mkConstTerms value vars 
    | valueInConsistent = 
        error $ "aern-poly: mkConstTerms: inconsistent coefficient"
    | otherwise = 
        aux vars
    where
    aux [] = IntPolyC value
    aux (var:rest) = IntPolyV var $ IntMap.singleton 0 (aux rest)
    valueInConsistent = 
        (isConsistentEff (consistencyDefaultEffort value) value) == Just False

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     RefOrd.IntervalLike cf)
    =>
    HasZero (IntPoly var cf)
    where
    zero sampleP = newConstFnFromSample sampleP $ zero sampleCf
        where
        sampleCf = getSampleDomValue sampleP
        
instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     RefOrd.IntervalLike cf)
    =>
    HasOne (IntPoly var cf)
    where
    one sampleP = newConstFnFromSample sampleP $ one sampleCf
        where
        sampleCf = getSampleDomValue sampleP
        
instance 
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     RefOrd.IntervalLike cf) => 
    (HasProjections (IntPoly var cf))
    where
    newProjection cfg _dombox var =
        IntPoly cfg $ mkProjTerms cfg var vars domsLE
        where
        vars = ipolycfg_vars cfg
        domsLE = ipolycfg_domsLE cfg
        
mkProjTerms :: 
    (Eq var, Show var, HasOne cf, 
     HasConsistency cf)
    =>
    IntPolyCfg _var cf -> 
    var -> 
    [var] -> 
    [cf] -> 
    IntPolyTerms var cf
mkProjTerms cfg var vars domsLE = aux vars domsLE
    where
    aux [] [] = 
        error $ 
            "IntPoly: newProjection: variable " ++ show var 
            ++ " not among specified variables " ++ show vars
    aux (cvar : restVars) (domLE : restDoms)
        | cvar == var = 
            IntPolyV var $ 
                IntMap.fromAscList $ 
                    [(1, mkConstTerms o restVars),
                     (0, mkConstTerms domLE restVars)]
        | otherwise = 
            IntPolyV cvar $ IntMap.singleton 0 (aux restVars restDoms)
        where
        o = one sampleCf
--        z = zero sampleCf
        sampleCf = ipolycfg_sample_cf cfg 
    aux _ _ = error "aern-intpoly internal error in New.mkProjTerms"
            
            

            