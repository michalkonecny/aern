{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Evaluation
    Description :  evaluation of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of interval polynomials in general using its instance of 'CanEvaluateOtherType' 
    and specilised cases for evaluation at a point and on an interval.
-}

module Numeric.AERN.Poly.IntPoly.Evaluation
    (
        PolyEvalOps(..),
        PolyEvalMonoOps(..)
--        ,
--        evalPolyAtPointOut,
--        evalPolyAtPointIn,
--        evalPolyOnIntervalOut,
--        evalPolyOnIntervalIn
    )
where

import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()
import Numeric.AERN.Poly.IntPoly.Differentiation

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.List (sortBy)

instance 
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    CanEvaluateOtherType (IntPoly var cf)
    where
    type EvalOps (IntPoly var cf) =  PolyEvalOps var cf
    evalOtherType ops valsMap p@(IntPoly cfg _) =
        case polyEvalMonoOps ops of
            Nothing -> evalDirect valsLZ p
            _ -> evalPolyMono evalDirect ops valsLZ p 
        where
        evalDirect = evalPolyDirect ops
        valsLZ = valsMapToValuesLZ subtrCf cfg valsMap
        subtrCf val domLE =
             addV val (cfV $ neg domLE)
        addV = polyEvalAdd ops
        cfV = polyEvalCoeff ops

instance 
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     HasAntiConsistency cf)
    =>
    CanEvaluateOtherTypeInner (IntPoly var cf)
    where
    evalOtherTypeInner ops valsMap p@(IntPoly cfg _) =
        case polyEvalMonoOps ops of
            Nothing -> evalDirect valsLZ p
            _ -> evalPolyMono evalDirect ops valsLZ p 
        where
        evalDirect vals p2 = 
            flipConsistency $
                evalPolyDirect ops vals $ 
                    flipConsistencyPoly p2
        valsLZ = valsMapToValuesLZ subtrCf cfg valsMap
        subtrCf val domLE =
             addV val (cfV $ neg domLE)
        addV = polyEvalAdd ops
        cfV = polyEvalCoeff ops
    

valsMapToValuesLZ :: 
    (Ord var) 
     =>
     (t -> cf -> t) ->
     IntPolyCfg var cf -> 
     (VarBox (IntPoly var cf) t) -> 
     [t]
valsMapToValuesLZ subtractCf cfg valsMap =
    zipWith subtractCf vals domsLE
    where
    vals = map getValue vars
    vars = ipolycfg_vars cfg
    domsLE = ipolycfg_domsLE cfg
    getValue var = 
        case lookupVar valsMap var of 
            Just val -> val
            _ -> error "aern-poly internal error in Evaluation...valsMapToValues"
     
instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf, 
     RefOrd.IntervalLike cf)
    =>
    CanEvaluate (IntPoly var cf)
    where
    type (EvaluationEffortIndicator (IntPoly var cf)) = 
        (ArithInOut.RoundedRealEffortIndicator cf, Int1To10)
    evaluationDefaultEffort (IntPoly cfg _) =
        (ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg),
         Int1To10 depth)
        where
        depth = 1 + (maxDeg `div` 2)
        maxDeg = ipolycfg_maxdeg cfg
    evalAtPointOutEff (effCf, Int1To10 maxSplitDepth) valsMap p@(IntPoly cfg _) 
        | valuesAreExact valsLZ =
            evalPolyAtPointOut effCf maxSplitDepth valsLZ p
        | otherwise =
            evalPolyOnIntervalOut effCf maxSplitDepth valsLZ p
        where
        valsLZ =
            let ?addInOutEffort = effAdd in 
            valsMapToValuesLZ (<->) cfg valsMap
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample effCf
        sample = ipolycfg_sample_cf cfg
        
    evalAtPointInEff (effCf, Int1To10 maxSplitDepth) valsMap p@(IntPoly cfg _) 
        | valuesAreExact valsLZ =
            evalPolyAtPointIn effCf maxSplitDepth valsLZ p
        | otherwise =
            evalPolyOnIntervalIn effCf maxSplitDepth valsLZ p
        where
        valsLZ = 
            let ?addInOutEffort = effAdd in 
            valsMapToValuesLZ (>-<) cfg valsMap
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample effCf
        sample = ipolycfg_sample_cf cfg
    
valuesAreExact :: 
    HasImprecision t 
    => 
    [t] -> Bool
valuesAreExact values =
    and $ map isCertainlyExact values
    where
    isCertainlyExact val = 
        isExactEff (imprecisionDefaultEffort val) val == Just True 

    
instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    HasEvalOps (IntPoly var cf) cf
    where
    type EvalOpsEffortIndicator (IntPoly var cf) cf = 
        (ArithInOut.RoundedRealEffortIndicator cf, Int1To10)
    evalOpsDefaultEffort p@(IntPoly cfg _) sampleCf = 
        (ArithInOut.roundedRealDefaultEffort sampleCf, Int1To10 depth)
        where
        depth = 1 + (maxDeg `div` 2)
        maxDeg = ipolycfg_maxdeg cfg
         
    evalOpsEff (eff, Int1To10 depth) _sampleP sampleCf =
        coeffPolyEvalOpsOut eff depth sampleCf

data PolyEvalOps var cf val =
    PolyEvalOps
    {
        polyEvalZero :: val,
        polyEvalAdd :: (val -> val -> val),
        polyEvalMult :: (val -> val -> val),
        polyEvalPow :: (val -> Int -> val), {-^ non-negative integer power -}
        polyEvalCoeff :: (cf -> val), {-^ coeff conversion -}
        polyEvalMaybePoly :: (IntPolyTerms var cf -> Maybe val), {-^ optional direct poly conversion -}
        polyEvalLeq :: (val -> val -> Maybe Bool),
        polyEvalSplitDepth :: Int,
        polyEvalMonoOps :: Maybe (PolyEvalMonoOps var cf val)
    }

data PolyEvalMonoOps var cf val =
    PolyEvalMonoOps
    {
        polyEvalMonoOuter :: PolyEvalOps var cf val,
        polyEvalMonoGetEndpoints :: val -> (val, val),
        polyEvalMonoFromEndpoints :: (val, val) -> val,
        polyEvalMonoIsExact :: val -> Bool,
        polyEvalMonoSplit :: val -> (val, val),
        polyEvalMonoMerge :: (val, val) -> val,
        polyEvalMonoGetWidthAsDouble :: val -> Double,
        polyEvalMonoCfEffortIndicator :: ArithInOut.RoundedRealEffortIndicator cf
    }


coeffPolyEvalOpsOut ::
    (RefOrd.IntervalLike cf, ArithInOut.RoundedReal cf)
    =>
   (ArithInOut.RoundedRealEffortIndicator cf) ->
   Int ->
   cf ->
   PolyEvalOps var cf cf
coeffPolyEvalOpsOut eff depth sample =
    result
    where
    result =
        let ?multInOutEffort = effMult in
        let ?intPowerInOutEffort = effPwr in
        let ?addInOutEffort = effAdd in
        let ?pCompareEffort = effComp in
        let ?joinmeetEffort = effJoin in
        PolyEvalOps (zero sample) (<+>) (<*>) (<^>) id (const Nothing) (<=?) depth $
            Just $ PolyEvalMonoOps
                result -- outer rounded ops = itself
                RefOrd.getEndpointsOutWithDefaultEffort
                RefOrd.fromEndpointsOutWithDefaultEffort
                isDefinitelyExact
                split
                (uncurry (</\>))
                getWidthAsDouble
                eff
    isDefinitelyExact a =
        (isExactEff $ ArithInOut.rrEffortImprecision a eff) a == Just True
    split val = (val1, val2)
        where
        val1 = RefOrd.fromEndpointsOutWithDefaultEffort (valL, valM)
        val2 = RefOrd.fromEndpointsOutWithDefaultEffort (valM, valR)
        (valL, valR) = RefOrd.getEndpointsOutWithDefaultEffort val
        valM =
            let ?mixedDivInOutEffort = effDivInt in
            let ?addInOutEffort = effAdd in
            (valL <+> valR) </>| (2 :: Int)
    getWidthAsDouble val = wD
        where
        Just wD = ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort val (0::Double)) w
        w = 
            let ?addInOutEffort = effAdd in
            valR <-> valL
        (valL, valR) = RefOrd.getEndpointsOutWithDefaultEffort val
        
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effDivInt = ArithInOut.mxfldEffortDiv sample (1::Int) $ ArithInOut.rrEffortIntMixedField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff


instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf, 
     Show cf)
    =>
    ArithUpDn.Convertible (IntPoly var cf) cf
    where
    type ArithUpDn.ConvertEffortIndicator (IntPoly var cf) cf = 
        (EvaluationEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort sampleP sampleCf = 
        (evaluationDefaultEffort sampleP, 
         RefOrd.getEndpointsDefaultEffort sampleCf)
    convertUpEff (effEval, effGetEndpts) p =
        Just $ snd $ RefOrd.getEndpointsOutEff effGetEndpts range
        where
        range = evalOtherType (evalOpsEff effEval sampleP sampleCf) varDoms p 
        sampleP = p
        sampleCf = getSampleDomValue sampleP
        varDoms = getDomainBox p
    convertDnEff (effEval, effGetEndpts) p =
        Just $ fst $ RefOrd.getEndpointsOutEff effGetEndpts range
        where
        range = evalOtherType (evalOpsEff effEval sampleP sampleCf) varDoms p 
        sampleP = p
        
        sampleCf = getSampleDomValue sampleP
        varDoms = getDomainBox p
    

evalPolyAtPointOut, evalPolyAtPointIn ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf, 
     Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    Int ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyAtPointOut eff depth values p@(IntPoly cfg _)
    = 
    evalPolyDirect (coeffPolyEvalOpsOut eff depth sample) values p
    where
    sample = ipolycfg_sample_cf cfg
evalPolyAtPointIn eff depth values p@(IntPoly cfg _)
    = 
    flipConsistency $
        evalPolyDirect (coeffPolyEvalOpsOut eff depth sample) values $ 
            flipConsistencyPoly p
    where
    sample = ipolycfg_sample_cf cfg

evalPolyOnIntervalOut, evalPolyOnIntervalIn ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, 
     HasAntiConsistency cf, 
     Show cf) 
    => 
    ArithInOut.RoundedRealEffortIndicator cf 
    ->
    Int
    ->
    [cf] {- values for each variable respectively -} 
    -> 
    IntPoly var cf -> cf
evalPolyOnIntervalOut eff maxSplitDepth values p@(IntPoly cfg _)
    = 
    evalPolyMono evalOut ops values p
    where
    evalOut = evalPolyDirect ops
    ops = coeffPolyEvalOpsOut eff maxSplitDepth sample 
    sample = ipolycfg_sample_cf cfg
evalPolyOnIntervalIn eff maxSplitDepth values p@(IntPoly cfg _)
    = 
    evalPolyMono evalIn ops values p
    where
    evalIn values p = 
        flipConsistency $ 
            evalPolyDirect ops values $ 
                flipConsistencyPoly p
    ops = coeffPolyEvalOpsOut eff maxSplitDepth sample 
    sample = ipolycfg_sample_cf cfg

evalPolyDirect ::
    (Ord var, Show var, Show cf, Show val, Neg cf) => 
    (PolyEvalOps var cf val) ->
    [val] ->
    IntPoly var cf -> val
evalPolyDirect opsV valuesLZ _p@(IntPoly _cfg terms)
    = 
--    unsafePrint
--    (
--        "evalPolyDirect: "
--        ++ "\n  values = " ++ show values
--        ++ "\n  p = " ++ show p
--    ) $
    ev valuesLZ terms
    where
    zV = polyEvalZero opsV
    addV = polyEvalAdd opsV
    multV = polyEvalMult opsV
    powV = polyEvalPow opsV
    cfV = polyEvalCoeff opsV
    polyV = polyEvalMaybePoly opsV
    ev [] (IntPolyC cf) = cfV cf
    ev (varValueLZ : restValues) (IntPolyV _ powers)
        | IntMap.null powers = zV
        | lowestExponent == 0 = 
            resultMaybeWithoutConstantTerm
        | otherwise =
            (varValueLZ `powV` lowestExponent) `multV` resultMaybeWithoutConstantTerm 
        where
        (lowestExponent, resultMaybeWithoutConstantTerm) 
            = IntMap.foldWithKey addTerm (highestExponent, zV) powers 
        (highestExponent, _) = IntMap.findMax powers
        addTerm exponent_2 poly (prevExponent, prevVal) = (exponent_2, newVal)
            where
            newVal = -- Horner scheme:
                polyValue 
                `addV`
                (prevVal `multV` (varValueLZ `powV` (prevExponent - exponent_2)))
            polyValue =
                case polyV poly of
                    Just value -> value
                    Nothing -> ev restValues poly  
    ev varVals terms_2 =
        error $ 
            "evalPolyDirect: illegal case:" 
            ++ "\n varVals = " ++ show varVals 
            ++ "\n terms = " ++ show terms_2

-- TODO: make the following function generic for any function representation with nominal derivatives    
evalPolyMono ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf,
     Show cf, Show val) 
    =>
    ([val] -> IntPoly var cf -> val) -- ^ direct evaluator (typically, @evalPolyDirect opsV@) 
    ->
    (PolyEvalOps var cf val) 
    ->
    [val] {-^ value for each variable -} 
    -> 
    IntPoly var cf 
    -> 
    val
evalPolyMono evalDirect opsV valuesG p@(IntPoly cfg _)
    | noMonoOps = direct
--    | noMonotoneVar = useMonotonicityAndSplit
    | otherwise =
--        unsafePrint
--        (
--            "evalPolyMono: "
----            ++ "\n p = " ++ show p
--            ++ "\n values = " ++ show values
--            ++ "\n valuesL = " ++ show valuesL
--            ++ "\n valuesR = " ++ show valuesR
--            ++ "\n left = " ++ show left
--            ++ "\n right = " ++ show right
--            ++ "\n direct = " ++ show direct
--        ) $ 
        useMonotonicityAndSplit
    where
    vars = ipolycfg_vars cfg
    
    direct = evalDirect valuesG p
    
    useMonotonicityAndSplit =
        useMonotonicityAndSplitWith maxSplitDepth $ zip valuesG $ repeat (False, False)
        
    useMonotonicityAndSplitWith remainingDepth valuesAndPrevDetectedMonotonicity
        | remainingDepth > 0 && splitHelps =
--            unsafePrint
--            (
--                "evalPolyMono: useMonotonicityAndSplitWith: SPLIT"
--                ++ "\n remainingDepth = " ++ show remainingDepth
--                ++ "\n valuesAndPrevDetectedMonotonicity = " ++ show valuesAndPrevDetectedMonotonicity
--                ++ "\n valuesAndCurrentDetectedMonotonicity = " ++ show valuesAndCurrentDetectedMonotonicity
--                ++ "\n nosplitResult = " ++ show nosplitResult
--                ++ "\n nosplitResultWidth = " ++ show nosplitResultWidth
--                ++ "\n bestSplit = " ++ show bestSplit
--                ++ "\n bestSplitResult = " ++ show _bestSplitResult
--                ++ "\n bestSplitWidth = " ++ show bestSplitWidth
--            ) $ 
            computeSplitResultContinueSplitting bestSplit
        | otherwise = 
--            unsafePrint
--            (
--                "evalPolyMono: useMonotonicityAndSplitWith: DONE"
--                ++ "\n remainingDepth = " ++ show remainingDepth
--                ++ "\n valuesAndPrevDetectedMonotonicity = " ++ show valuesAndPrevDetectedMonotonicity
--                ++ "\n valuesAndCurrentDetectedMonotonicity = " ++ show valuesAndCurrentDetectedMonotonicity
--                ++ "\n nosplitResult = " ++ show nosplitResult
--                ++ "\n nosplitResultWidth = " ++ show nosplitResultWidth
--            ) $ 
            nosplitResult
        where
        (nosplitResult, valuesAndCurrentDetectedMonotonicity) = 
            useMonotonicity valuesAndPrevDetectedMonotonicity
        nosplitResultWidth = getWidthDblV nosplitResult
            
        splitHelps 
            | null possibleSplits = False
            | otherwise = bestSplitWidth < nosplitResultWidth
        (((bestSplitWidth, _bestSplitResult), bestSplit) : _) =
            sortBy (\ ((a,_),_) ((b,_),_) -> compare a b) $ zip splitWidths possibleSplits
        splitWidths =
            map getWidth splitResults
            where
            getWidth result = (width :: Double, result)
                where
                width = getWidthDblV result 
        splitResults =
            map computeSplitResult possibleSplits
        possibleSplits =
            getSplits [] [] valuesAndCurrentDetectedMonotonicity
            where
            getSplits prevSplits _prevValues [] = prevSplits
            getSplits prevSplits prevValues (vd@(value, dmAndIncr@(detectedMono, _)) : rest) 
                | detectedMono =
                    getSplits prevSplits (vd : prevValues) rest -- do not split value for which p is monotone
                | otherwise =
                    getSplits (newSplit : prevSplits) (vd : prevValues) rest
                where
                newSplit =
                    (
                        prevValuesRev ++ [(valueL, dmAndIncr)] ++ rest
                    ,
                        prevValuesRev ++ [(valueR, dmAndIncr)] ++ rest
                    )
                prevValuesRev = reverse prevValues
                (valueL, valueR) = splitV value
        
        computeSplitResult (valuesAndDM_L, valuesAndDM_R) =
            mergeV (resultL, resultR)
            where
            (resultL, _) = useMonotonicity valuesAndDM_L
            (resultR, _) = useMonotonicity valuesAndDM_R
        
        computeSplitResultContinueSplitting (valuesAndDM_L, valuesAndDM_R) =
            mergeV (resultL, resultR)
            where
            resultL = useMonotonicityAndSplitWith (remainingDepth - 1) valuesAndDM_L
            resultR = useMonotonicityAndSplitWith (remainingDepth - 1) valuesAndDM_R
                    
        
    useMonotonicity valuesAndPrevDetectedMonotonicity =
        (fromEndPtsV (left, right), valuesAndCurrentDetectedMonotonicity)
        where
        values = map fst valuesAndPrevDetectedMonotonicity
        left = evalDirect valuesL p
        right = evalDirect valuesR p
        (_noMonotoneVar, valuesL, valuesR, valuesAndCurrentDetectedMonotonicity) =
            let ?mixedMultInOutEffort = effMult in
            detectMono True [] [] [] $ 
                reverse $  -- undo reverse due to the accummulators
                    zip vars $ valuesAndPrevDetectedMonotonicity
        detectMono noMonotoneVarPrev valuesLPrev valuesRPrev 
                valuesAndCurrentDetectedMonotonicityPrev []
            = (noMonotoneVarPrev, valuesLPrev, valuesRPrev, 
                    valuesAndCurrentDetectedMonotonicityPrev)
        detectMono noMonotoneVarPrev valuesLPrev valuesRPrev 
                valuesAndCurrentDetectedMonotonicityPrev ((var, (val, dmAndIncr@(prevDM, isIncreasing))) : rest)
            =
--            unsafePrint 
--            (
--                "evalPolyMono: detectMono: deriv = " ++ show deriv
--            ) $ 
            detectMono noMonotoneVarNew (valLNew : valuesLPrev) (valRNew : valuesRPrev) 
                 ((val, newDMAndIncr) : valuesAndCurrentDetectedMonotonicityPrev) rest
            where
            noMonotoneVarNew = noMonotoneVarPrev && varNotMono
            (valLNew, valRNew, newDMAndIncr)
                | prevDM && isIncreasing = (valL, valR, dmAndIncr)
                | prevDM = (valR, valL, dmAndIncr)
                | varNotMono = (val, val, dmAndIncr) -- not monotone, we have to be safe
                | varNonDecr = (valL, valR, (True, True)) -- non-decreasing on the whole domain - can use endpoints
                | otherwise = (valR, valL, (True, False)) -- non-increasing on the whole domain - can use swapped endpoints
            (varNonDecr, varNotMono) =
                case (valIsExact, zV `leqV` deriv, deriv `leqV` zV) of
                    (True, _, _) -> (undefined, True) -- when a variable has a thin domain, do not bother separating endpoints 
                    (_, Just True, _) -> (True, False) 
                    (_, _, Just True) -> (False, False)
                    _ -> (undefined, True)
            deriv =
                evalPolyDirect opsVOut values $ -- this evaluation must be outer-rounded!
                    diffPolyOut effCf var p -- range of (d p)/(d var)    
            (valL, valR) = getEndPtsV val
            valIsExact = isExactV val

    zV = polyEvalZero opsV
    leqV = polyEvalLeq opsV
    maxSplitDepth = polyEvalSplitDepth opsV 
    (noMonoOps, monoOpsV) = 
        case polyEvalMonoOps opsV of
            Nothing -> (True, error "evalPolyMono: internal error: monoOpsV used when not present")
            Just monoOpsV_2 -> (False, monoOpsV_2)
    fromEndPtsV = polyEvalMonoFromEndpoints monoOpsV
    getEndPtsV = polyEvalMonoGetEndpoints monoOpsV
    isExactV = polyEvalMonoIsExact monoOpsV
    splitV = polyEvalMonoSplit monoOpsV
    mergeV = polyEvalMonoMerge monoOpsV
    getWidthDblV = polyEvalMonoGetWidthAsDouble monoOpsV
    
    effCf = polyEvalMonoCfEffortIndicator monoOpsV
    opsVOut = polyEvalMonoOuter monoOpsV
    
    effMult = ArithInOut.mxfldEffortMult sampleCf (1::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf
    sampleCf = ipolycfg_sample_cf cfg
    

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf, 
     RefOrd.IntervalLike cf)
    =>
    CanPartiallyEvaluate (IntPoly var cf)
    where
    type (PartialEvaluationEffortIndicator (IntPoly var cf)) = 
        ArithInOut.RoundedRealEffortIndicator cf
    partialEvaluationDefaultEffort (IntPoly cfg _) =
        ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg)
    pEvalAtPointOutEff = partiallyEvalPolyAtPointOut
    pEvalAtPointInEff =
        error "aern-poly: no inwards-rounded partial evaluation for IntPoly" 
    
partiallyEvalPolyAtPointOut ::
    (Ord var, ArithInOut.RoundedReal cf) 
    =>
    ArithInOut.RoundedRealEffortIndicator cf -> 
    Map.Map var cf -> 
    IntPoly var cf -> 
    IntPoly var cf
partiallyEvalPolyAtPointOut effCf valsMap _p@(IntPoly cfg terms) =
    {- TODO: currently there is massive dependency effect here,
        move this to Composition and deal with it as a special case
        of composition
    -}
    IntPoly cfgVarsRemoved $ pev domsLE terms
    where
    cfgVarsRemoved =
        cfg
        {
            ipolycfg_vars = newVars,
            ipolycfg_domsLE = newDomsLE,
            ipolycfg_domsLZ = newDomsLZ
        }
        where
        (newVars, newDomsLE, newDomsLZ)
            = unzip3 $ filter notSubstituted $ zip3 vars domsLE domsLZ
            where
            vars = ipolycfg_vars cfg
            domsLZ = ipolycfg_domsLZ cfg
            notSubstituted (var, _, _) =
                var `Map.notMember` valsMap
    domsLE = ipolycfg_domsLE cfg
    pev _ t@(IntPolyC _) = t
    pev (domLE : restDomsLE) (IntPolyV var powers) =
        case Map.lookup var valsMap of
            Just value ->
                let ?addInOutEffort = effAdd in
                -- evaluate evaluatedPowers using the Horner scheme: 
                foldl (addAndScale (value <-> domLE) highestExponent) heTerms lowerPowers
            _ ->
                IntPolyV var evaluatedPowers
        where
        evaluatedPowers =
            IntMap.map (pev restDomsLE) powers
        ((highestExponent, heTerms) : lowerPowers) =  
            reverse $ IntMap.toAscList evaluatedPowers
        addAndScale value prevExponent termsSoFar (currExponent, currTerms) =
            let ?multInOutEffort = effMult in
            addTermsAux currTerms $ termsMapCoeffs (<*> valuePower) termsSoFar
            where
            valuePower = 
                let ?intPowerInOutEffort = effPow in
                value <^> (prevExponent - currExponent)  
            addTermsAux (IntPolyV v powers1) (IntPolyV _ powers2) =
                IntPolyV v $ IntMap.unionWith addTermsAux powers1 powers2
            addTermsAux (IntPolyC val1) (IntPolyC val2) = 
                let ?addInOutEffort = effAdd in
                IntPolyC $ val1 <+> val2 
        effAdd = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        effMult = ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        effPow = ArithInOut.fldEffortPow sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        sampleCf = domLE
                