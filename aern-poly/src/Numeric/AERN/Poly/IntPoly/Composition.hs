{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Composition
    Description :  composition of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of interval polynomials in general using its instance of 'CanEvaluateOtherType' 
    and specilised cases for evaluation at a point and on an interval.
-}

module Numeric.AERN.Poly.IntPoly.Composition
where

import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()
import Numeric.AERN.Poly.IntPoly.Evaluation
import Numeric.AERN.Poly.IntPoly.Show ()
import Numeric.AERN.Poly.IntPoly.NumericOrder ()
import Numeric.AERN.Poly.IntPoly.RefinementOrder ()
import Numeric.AERN.Poly.IntPoly.Addition ()
import Numeric.AERN.Poly.IntPoly.Multiplication ()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort ((<+>|))
--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
--import qualified Data.Map as Map

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    HasEvalOps (IntPoly var cf) (IntPoly var cf)
    where
    type EvalOpsEffortIndicator (IntPoly var cf) (IntPoly var cf) = 
        (Int1To1000, ArithInOut.RoundedRealEffortIndicator cf)
    evalOpsDefaultEffort _ sampleP = 
        NumOrd.pCompareDefaultEffort sampleP
    evalOpsOut eff _ sampleP =
        polyPolyEvalOpsOut eff sampleP sampleCf
        where
        sampleCf = getSampleDomValue sampleP
    evalOpsIn =
        error "aern-poly: inner-roudned composition not supported for IntPoly"

polyPolyEvalOpsOut ::
    (Ord var, Show var, Show cf,
     RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf),
     Show (Imprecision cf))
    =>
   (Int1To1000, ArithInOut.RoundedRealEffortIndicator cf) ->
   (IntPoly var cf) ->
   cf ->
   PolyEvalOps var cf (IntPoly var cf)
polyPolyEvalOpsOut effCmp@(_,effCf) sampleP sampleCf =
    let (<+>) = ArithInOut.addOutEff effCf in
    let (<*>) = ArithInOut.multOutEff effCf in
    let (<^>) = ArithInOut.powerToNonnegIntOutEff effCf in
    let (<=?) = NumOrd.pLeqEff effCmp in
    PolyEvalOps (zero sampleP) (<+>) (<*>) (<^>) (newConstFnFromSample sampleP) (const Nothing) (<=?) $
        Just $ PolyEvalMonoOps
            RefOrd.getEndpointsOutWithDefaultEffort
            RefOrd.fromEndpointsOutWithDefaultEffort
            isDefinitelyExact
            effCf
    where
    isDefinitelyExact p = 
        polyIsExactEff effImpr p == Just True
    effImpr = ArithInOut.rrEffortImprecision sampleCf effCf
--    effAddCf = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        
instance 
    (Ord var, Show var, 
     Show cf,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf),
     RefOrd.IntervalLike cf)
    => 
    (CanCompose (IntPoly var cf))
    where
    type CompositionEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf
    compositionDefaultEffort (IntPoly cfg _) =
        ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg
--    composeVarsOutEff = polyComposeAllVarsOutEff
    composeVarOutEff = polyComposeVarOutEff
        
polyComposeVarOutEff eff substVar substPoly p@(IntPoly cfg _) =
    result
    where
    vars = ipolycfg_vars cfg
    result =
        evalOtherType ops varSubstBox p
        where
        varSubstBox =
            fromList $ map makeVarSubst vars
        makeVarSubst var
            | var == substVar = (var, substPoly)
            | otherwise = (var,  proj)
            where
            proj =
                newProjectionFromSample substPoly var
    ops =
        polyPolyEvalOpsOut (Int1To1000 0, eff) sampleP sampleCf
    sampleP = substPoly
    sampleCf = getSampleDomValue substPoly
    
{- 
    the following are optimized versions of the above; 
    TODO: let the above detect the special situations and use these optimizations 
-}
        
polyComposeMainVarKeepOutEff ::
    (Ord var, Show var, 
     Show cf,
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf),
     RefOrd.IntervalLike cf)
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    IntPoly var cf {- polynomial to substitute the main var with -} ->
    IntPoly var cf -> 
    IntPoly var cf
polyComposeMainVarKeepOutEff eff substPoly p@(IntPoly cfg _) =
--    unsafePrint
--    (
--        "substPolyMainVar: "
--        ++ "\n p = " ++ showP p
--        ++ "\n substPoly = " ++ showP substPoly
--        ++ "\n result = " ++ showP result
--    ) $
    result
    where
    (mainVar : otherVars) = ipolycfg_vars cfg
    result =
        evalOtherType opsMainVarOnly varvaluesBox p
        where
        varvaluesBox =
            fromList $ (mainVar, substPoly) : otherVarsSubsts
        otherVarsSubsts =
            map makeVarSubst $ zip otherVars $ tail $ ipolycfg_domsLE cfg
        makeVarSubst (var, domLE) =
            (var,  proj)
            where
            proj =
                newProjectionFromSample p var
    opsMainVarOnly =
        (polyPolyEvalOpsOut (Int1To1000 0, eff) sampleP sampleCf)
            {
                polyEvalMaybePoly = polyEvalCopySubPolynomials
            }
    polyEvalCopySubPolynomials subterms 
        | isMainVar = Nothing
        | otherwise =
            Just $
                IntPoly cfg $ IntPolyV mainVar $ IntMap.singleton 0 subterms
        where
        isMainVar =
            case subterms of
                (IntPolyV var _) -> var == mainVar
                _ -> False
            
    sampleP = p
    sampleCf = getSampleDomValue p


polyComposeMainVarElimOutEff ::
    (Ord var, Show var, 
     Show cf,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf),
     RefOrd.IntervalLike cf)
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    IntPoly var cf {- polynomial to substitute the main var with -} ->
    IntPoly var cf -> 
    IntPoly var cf
polyComposeMainVarElimOutEff eff substPoly p@(IntPoly cfg _) =
--    unsafePrint
--    (
--        "substPolyMainVar: "
--        ++ "\n p = " ++ showP p
--        ++ "\n substPoly = " ++ showP substPoly
--        ++ "\n result = " ++ showP result
--    ) $
    result
    where
    (mainVar : otherVars) = ipolycfg_vars cfg
    result =
        evalOtherType opsMainVarOnly varvaluesBox p
        where
        varvaluesBox =
            fromList $ (mainVar, substPoly) : otherVarsSubsts
        otherVarsSubsts =
            map makeVarSubst otherVars
        makeVarSubst var =
            (var, newProjectionFromSample sampleP var)
    opsMainVarOnly =
        (polyPolyEvalOpsOut (Int1To1000 0, eff) sampleP sampleCf)
            {
                polyEvalMaybePoly = polyEvalCopySubPolynomials
            }
    polyEvalCopySubPolynomials subterms 
        | isMainVar = Nothing
        | otherwise =
            Just $ IntPoly cfgR subterms
        where
        cfgR = cfgRemVar cfg
        isMainVar =
            case subterms of
                (IntPolyV var _) -> var == mainVar
                _ -> False
            
    sampleP = substPoly
    sampleCf = getSampleDomValue p


