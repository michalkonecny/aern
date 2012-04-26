{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Splitting
    Description :  adaptive splitting solver parametrised by a single-step solver  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Adaptive splitting solver parametrised by a single-step solver.
    
    Typically one uses solveODEIVPBySplittingT0 and
    its subsolver is defined using solveODEIVPBySplittingAtT0End
    and its subsolver for the right-hand side is 
    defined using solveODEIVPBySplittingT:
    
    solveODEIVPBySplittingT0
    |
    |
    solveODEIVPBySplittingAtT0End
    |       \
    |        \
    solve-VT  solveODEIVPBySplittingT
              |
              |
              solve-Vt
-}

module Numeric.AERN.IVP.Solver.Splitting
(
    solveHybridIVPBySplittingT,
    solveODEIVPBySplittingAtT0End,
    solveODEIVPBySplittingT,
    solveODEIVPBySplittingT0,
    SplittingInfo(..),
    showSplittingInfo,
    splittingInfoCountLeafs
)
where

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Specification.Hybrid

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Basics.Exception
--import Control.Exception (throw)

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        
solveHybridIVPBySplittingT ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f),
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     Show f, Show (Domain f))
    =>
    (Int -> HybridIVP f -> (Maybe (HybridSystemUncertainState f), solvingInfo)) -- ^ solver to use for segments  
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    Imprecision (Domain f) -- ^ splitting improvement threshold
    ->
    Domain f -- ^ minimum segment length  
    ->
    (HybridIVP f)  -- ^ problem to solve
    ->
    (
        Maybe (HybridSystemUncertainState f)
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        )
    )
solveHybridIVPBySplittingT
        solver
            effDom splitImprovementThreshold minStepSize 
                hybivpG 
    =
    result
    where
    result = splitSolve 0 hybivpG
    
    splitSolve depth hybivp =
--        unsafePrint
--        (
--            "solveHybridIVPBySplittingT: splitSolve: "
--            ++ "tStart = " ++ show tStart
--            ++ "tEnd = " ++ show tEnd
--        ) $
        result2
        where
        result2
            | belowStepSize = directComputation
            | directComputationFailed = splitComputation
            | otherwise = 
                case maybeSplitImprovement of
                    Just improvementBy 
                        | (improvementBy >? splitImprovementThreshold) /= Just True -> 
                            directComputation -- computations succeeded but brought no noticeable improvement
                    _
                        | splitComputationFailed -> directComputation
                        | otherwise -> splitComputation -- splitting either brought noticeable improvement or some computation failed 
        tStart = hybivp_tStart hybivp
        tEnd = hybivp_tEnd hybivp
        
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((tEnd <-> tStart) >? minStepSize) /= Just True

        directComputation =
--            unsafePrint
--            (
--                "solveHybridIVPBySplittingT: completed time " ++ show tEnd
--            ) $
            case maybeDirectResult of
                Just resultOut 
                    | otherwise -> (Just resultOut, SegNoSplit directInfo)
                _ -> (Nothing, SegNoSplit directInfo) 
        (maybeDirectResult, directInfo) = solver depth hybivp
        directComputationFailed =
            case maybeDirectResult of Just _ -> False; _ -> True
        
        splitOnceComputation = -- needed only to decide whether splitting is benefitial, the result is then discarded
            case solver (depth + 1) hybivpL of
                (Just midState, _) ->
                    case solver (depth + 1) hybivpR of
                        (Just endStateOut, _) -> Just endStateOut 
                        _ -> Nothing
                    where
                    hybivpR =
                        hybivp
                        {
                            hybivp_tStart = tMid,
                            hybivp_initialStateEnclosure = midState
                        }
                _ -> Nothing
                
        (splitComputation, splitComputationFailed) =
            (
                (maybeState, SegSplit (directInfo, maybeSplitImprovement) infoL maybeInfoR)
            , 
                case maybeState of Just _ -> False; _ -> True
            )
            where
            (maybeMidState, infoL) =
                splitSolve (depth + 1) hybivpL
            (maybeState, maybeInfoR) =
                case maybeMidState of
                    Just midState ->
                        case splitSolve (depth + 1) hybivpR of
                            (maybeState2, infoR) -> (maybeState2, Just infoR)
                        where
                        hybivpR =
                            hybivp
                            {
                                hybivp_tStart = tMid,
                                hybivp_initialStateEnclosure = midState
                            }
                    Nothing -> (Nothing, Nothing)
        hybivpL =
            hybivp
            {
                hybivp_tEnd = tMid
            }
        tMid = 
            let ?addInOutEffort = effAddDom in
            let ?mixedDivInOutEffort = effDivDomInt in
            (tStart <+> tEnd) </>| (2 :: Int)
        
        maybeSplitImprovement =
            case (maybeDirectResult, splitOnceComputation) of
                (Just directResult, Just splitOnceResult) -> 
                    Just $ measureImprovementState directResult splitOnceResult
                _ -> Nothing
        measureImprovementState state1 state2 = 
            improvement 
            where
            improvement =
                let ?addInOutEffort = effAddDom in
                modeImprovement <+> (foldl1 (NumOrd.maxOutEff effMinmax) improvements)
            improvements = map measureImprovement $ zip vec1 vec2
            modeImprovement 
                | modes1 /= modes2 = one sampleDom
                | otherwise = zero sampleDom
            vec1 = hybstate_values state1
            vec2 = hybstate_values state2
            modes1 = hybstate_modes state1
            modes2 = hybstate_modes state2
        measureImprovement (encl1, encl2) =
            let ?addInOutEffort = effAddDom in
            (imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)

    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effDivDomInt = 
        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
--    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
    sampleDom = hybivp_tStart hybivpG
    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDom effDom
    
    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
--    sampleImpr = imprecisionOfEff effImpr sampleDom
--    effAddImpr = ArithInOut.fldEffortAdd sampleImpr $ ArithInOut.rrEffortImprecisionField sampleDom effDom
        
solveODEIVPBySplittingAtT0End ::
    (HasAntiConsistency (Domain f), 
     HasAntiConsistency f,
     Show f, Show (Domain f))
    =>
    (ODEIVP f -> (Maybe [f], solvingInfoL)) -- ^ uncertain time solver; giving parametrised results
    -> 
    ([f] -> ODEInitialValues f) -- ^ make ODE IVP initial value specification from parametrised initial values
    -> 
    (ODEIVP f -> (Maybe [Domain f], solvingInfoR)) -- ^ exact time solver; giving wrapped results
    -> 
    ODEIVP f
    -> 
    (
        Maybe [Domain f]
    , 
        (solvingInfoL, Maybe solvingInfoR)
    )
solveODEIVPBySplittingAtT0End
        solverVT makeMakeParamInitValFnVec solverVt 
            odeivpG 
    =
    (maybeResult, (infoL, maybeInfoR))
    where
    (maybeResult, maybeInfoR) =
        case maybeResultL of
            Just fnVecLOut ->
                case maybeResultROut of
                    Just resultOut ->
--                        unsafePrint
--                        (
--                            "solveODEIVPBySplittingAtT0End:"
--                            ++ "\n fnVecLOut = " ++ show fnVecLOut
--                        )
                        (Just result, Just infoROut)
                        where
                        result = resultOut 
                    Nothing -> (Nothing, Just infoROut)
                where
                (maybeResultROut, infoROut) = solverVt odeivpROut
                odeivpROut = odeivpR fnVecLOut
                odeivpR fnVecL =
                    odeivpG
                        {
                            odeivp_tStart = t0End
                        ,
                            odeivp_makeInitialValueFnVec =
                                makeMakeParamInitValFnVec fnVecL
                        }
            _ -> 
                (Nothing, Nothing)
    (maybeResultL, infoL) = solverVT odeivpL
    odeivpL =
        odeivpG
            {
                odeivp_tEnd = t0End
            }
    t0End = odeivp_t0End odeivpG
    
solveODEIVPBySplittingT ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f),
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     Show f, Show (Domain f))
    =>
    (ODEIVP f -> (Maybe result, solvingInfo)) -- ^ solver to use for segments  
    ->
    (result -> Domain f) -- ^ measure imprecision
    ->
    (result -> ODEInitialValues f) -- ^ how to change initial conditions
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    (Domain f) -- ^ splitting improvement threshold
    ->
    Domain f -- ^ minimum segment length  
    ->
    (ODEIVP f)  -- ^ problem to solve
    ->
    (
        Maybe result
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        )
    )
solveODEIVPBySplittingT
        solver measureResultImprecision makeMakeInitValFnVec
            effDom splitImprovementThreshold minStepSize 
                odeivpG 
    =
    result
    where
    result = splitSolve odeivpG
    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effDivDomInt = 
        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
    sampleDom = odeivp_tStart odeivpG

--    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
--    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDom effDom
--    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
--    sampleImpr = imprecisionOfEff effImpr sampleDom
--    effAddImpr = ArithInOut.fldEffortAdd sampleImpr $ ArithInOut.rrEffortImprecisionField sampleDom effDom
    
    splitSolve odeivp =
--        unsafePrint
--        (
--            "solveODEIVPBySplittingT: splitSolve: "
--            ++ "shouldRoundInwards = " ++ show shouldRoundInwards
--            ++ "tStart = " ++ show tStart
--            ++ "tEnd = " ++ show tEnd
--        ) $
        results
        where
        results
            | belowStepSize = directComputation
            | directComputationFailed = splitComputation
            | otherwise = 
                case maybeSplitImprovement of
                    Just improvementBy 
                        | (improvementBy >? splitImprovementThreshold) == Just True -> 
                            splitComputation
                    _ -> directComputation
        tStart = odeivp_tStart odeivp
        tEnd = odeivp_tEnd odeivp
        
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((tEnd <-> tStart) >? minStepSize) /= Just True

        directComputation =
            case maybeDirectResult of
                Just resultOut -> (Just resultOut, SegNoSplit directInfo)
                _ -> (Nothing, SegNoSplit directInfo) 
        (maybeDirectResult, directInfo) = solver odeivp
        directComputationFailed =
            case maybeDirectResult of Just _ -> False; _ -> True
        
        splitOnceComputation = -- needed only to decide whether splitting is benefitial, the result is then discarded
            case solver odeivpL of
                (Just midValuesOut, _) ->
                    case solver odeivpR of
                        (Just endValuesOut, _) -> Just endValuesOut 
                        _ -> Nothing
                    where
                    midValues = midValuesOut
                    odeivpR =
                        odeivp
                        {
                            odeivp_tStart = tMid,
                            odeivp_t0End = tMid, -- exact initial time
                            odeivp_makeInitialValueFnVec =
                                makeMakeInitValFnVec midValues 
                        }
                _ -> Nothing
                
        splitComputation =
            (maybeState, SegSplit (directInfo, maybeSplitImprovement) infoL maybeInfoR)
            where
            (maybeMidState, infoL) =
                splitSolve odeivpL
            (maybeState, maybeInfoR) =
                case maybeMidState of
                    Just midState ->
                        case splitSolve odeivpR of
                            (maybeState2, infoR) -> (maybeState2, Just infoR)
                        where
                        odeivpR =
                            odeivp
                            {
                                odeivp_tStart = tMid,
                                odeivp_t0End = tMid, -- exact initial time
                                odeivp_makeInitialValueFnVec =
                                    makeMakeInitValFnVec  midState
                            }
                    Nothing -> (Nothing, Nothing)
        odeivpL =
            odeivp
            {
                odeivp_tEnd = tMid
            }
        tMid = 
            let ?addInOutEffort = effAddDom in
            let ?mixedDivInOutEffort = effDivDomInt in
            (tStart <+> tEnd) </>| (2 :: Int)
        
        maybeSplitImprovement =
            case (directComputation, splitOnceComputation) of
                ((Just directResult, _), Just splitOnceResult) -> 
                        measureImprovementVec directResult splitOnceResult
                _ -> Nothing

        measureImprovementVec res1 res2 =
            let ?addInOutEffort = effAddDom in
            Just $ imprecision1 <-> imprecision2
            where
            imprecision1 = measureResultImprecision res1
            imprecision2 = measureResultImprecision res2
--        measureImprovementVec vec1 vec2 = 
--            do
--            improvements <- sequence $ 
--                                map measureImprovement $ 
--                                    zip vec1 vec2
--            Just $ foldl1 (NumOrd.minOutEff effMinmax) improvements
--        measureImprovement (encl1, encl2) =
--            let ?addInOutEffort = effAddDom in
--            let ?pCompareEffort = effRefComp in
--            do
----            refines <- encl1 |<=? encl2
----            case refines of
----                True -> 
--                    Just $ (imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)
----                False -> Nothing 
                
solveODEIVPBySplittingT0 ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f),
     Domain f ~ Imprecision (Domain f),
     solvingInfoSub ~ ((Domain f, Maybe [Domain f]), solvingInfo),
     Show f, Show (Domain f))
    =>
    (ODEIVP f -> (Maybe [Domain f], solvingInfo)) -- ^ solver to use for segments  
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    Imprecision (Domain f) -- ^ splitting improvement threshold
    ->
    Domain f -- ^ minimum segment length  
    ->
    (ODEIVP f)  -- ^ problem to solve
    ->
    (
        Maybe [Domain f]
    , 
        SplittingInfo solvingInfoSub (solvingInfoSub, Maybe (Imprecision (Domain f)))
    )
solveODEIVPBySplittingT0
        solver
            effDom splitImprovementThreshold minStepSize 
                odeivpG 
    =
    splitSolve odeivpG
    where
    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effDivDomInt = 
        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
    sampleDom = odeivp_tStart odeivpG
    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDom effDom
    effJoinDom = ArithInOut.rrEffortJoinMeet sampleDom effDom
    
    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
--    sampleImpr = imprecisionOfEff effImpr sampleDom
--    effAddImpr = ArithInOut.fldEffortAdd sampleImpr $ ArithInOut.rrEffortImprecisionField sampleDom effDom
    
    splitSolve odeivp
        | belowStepSize = directComputation
        | directComputationFailed = splitComputation
        | otherwise = 
            case maybeSplitImprovement of
                Just improvementBy 
                    | (improvementBy >? splitImprovementThreshold) == Just True -> 
                        splitComputation
                _ -> directComputation
        where
        tStart = odeivp_tStart odeivp
        t0End = odeivp_t0End odeivp
        tEnd = odeivp_tEnd odeivp
        
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((t0End <-> tStart) >? minStepSize) /= Just True

        directComputation =
            (maybeDirectResult, SegNoSplit ((tEnd, maybeDirectResult), directInfo))
        (maybeDirectResult, directInfo) = solver odeivp
        directComputationFailed =
            case maybeDirectResult of Just _ -> False; _ -> True
        
        splitOnceComputation = -- needed only to decide whether splitting is benefitial, the result is then discarded
            case solver odeivpL of
                (Just endValuesLOut, _) -> 
                    case solver odeivpR of
                        (Just endValuesROut, _) ->
                            Just endValuesOut
                            where
                            endValuesOut =
                                let ?joinmeetEffort = effJoinDom in
                                zipWith (</\>) endValuesLOut endValuesROut
                        _ -> Nothing
                    where
                    odeivpR =
                        odeivp
                        {
                            odeivp_tStart = t0Mid
                        }
                _ -> Nothing

        splitComputation =
            case splitSolve odeivpL of
                (Just endValuesLOut, infoL) -> 
                    case splitSolve odeivpR of
                        (Just endValuesROut, infoR) ->
                            (Just endValuesOut, SegSplit (((tEnd, maybeDirectResult), directInfo), maybeSplitImprovement) infoL (Just infoR))
                            where
                            endValuesOut =
                                let ?joinmeetEffort = effJoinDom in
                                zipWith (</\>) endValuesLOut endValuesROut
                        (Nothing, infoR) ->
                            (Nothing, SegSplit (((tEnd, maybeDirectResult), directInfo), maybeSplitImprovement) infoL (Just infoR))
                    where
                    odeivpR =
                        odeivp
                        {
                            odeivp_tStart = t0Mid 
                        }
                failedLeftComputation -> failedLeftComputation

        odeivpL =
            odeivp
            {
                odeivp_t0End = t0Mid
            }
        t0Mid = 
            let ?addInOutEffort = effAddDom in
            let ?mixedDivInOutEffort = effDivDomInt in
            (tStart <+> t0End) </>| (2 :: Int)
        
        maybeSplitImprovement =
            case (directComputation, splitOnceComputation) of
                ((Just directResult, _), Just splitOnceResult) ->
--            case (directComputation, splitComputation) of
--                ((Just (directResult, _), _), (Just (splitOnceResult, _), _)) ->
                    measureImprovementVec directResult splitOnceResult
                _ -> Nothing
        measureImprovementVec vec1 vec2 =
            do
            improvements <- sequence $ zipWith measureImprovement vec1 vec2
            Just $ foldl1 (NumOrd.minOutEff effMinmax) improvements
        measureImprovement encl1 encl2 =
            let ?addInOutEffort = effAddDom in
            let ?pCompareEffort = effRefComp in
            do
--            refines <- encl1 |<=? encl2
--            case refines of
--                True -> 
                    Just $ (imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)
--                False -> Nothing 

data SplittingInfo segInfo splitReason
    = SegNoSplit segInfo
    | SegSplit splitReason (SplittingInfo segInfo splitReason) (Maybe (SplittingInfo segInfo splitReason))

showSplittingInfo :: 
    (String -> segInfo -> String) 
    -> 
    (String -> splitReason -> String) 
    -> 
    String -> SplittingInfo segInfo splitReason -> String
showSplittingInfo showSegInfo showSplitReason indentG splittingInfoG =
    shLevel indentG splittingInfoG
    where
    shLevel indent splittingInfo =
        case splittingInfo of
            SegNoSplit segInfo -> showSegInfo indent segInfo
            SegSplit reason infoL Nothing ->
                (showSplitReason indent reason)
                ++ "\n" ++
                (shLevel (indent ++ "| ") infoL)
            SegSplit reason infoL (Just infoR) ->
                (showSplitReason indent reason)
                ++ "\n" ++
                (shLevel (indent ++ "| ") infoL)
                ++ "\n" ++
                (shLevel (indent ++ "  ") infoR)
             
splittingInfoCountLeafs ::
    SplittingInfo segInfo splitReason -> Int
splittingInfoCountLeafs (SegNoSplit _) = 1
splittingInfoCountLeafs (SegSplit _ left Nothing) =
    splittingInfoCountLeafs left
splittingInfoCountLeafs (SegSplit _ left (Just right)) =
    splittingInfoCountLeafs left + splittingInfoCountLeafs right
     