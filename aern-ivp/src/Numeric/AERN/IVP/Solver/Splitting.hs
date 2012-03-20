{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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
-}

module Numeric.AERN.IVP.Solver.Splitting
(
    solveODEIVPBySplittingAtT0End,
    solveODEIVPBySplittingT,
    solveODEIVPBySplittingT0,
    SplittingInfo(..),
    showSplittingInfo
)
where

import Numeric.AERN.IVP.Specification.ODE

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        
solveODEIVPBySplittingAtT0End ::
    (HasAntiConsistency (Domain f), Show (Domain f))
    =>
    (ODEIVP f -> (Maybe ([Domain f], [Domain f]), solvingInfoL))
    -> 
    ([Domain f] -> ODEInitialValues f)
    -> 
    (ODEIVP f -> (Maybe ([Domain f], [Domain f]), solvingInfoR))
    -> 
    ODEIVP f
    -> 
    (Maybe ([Domain f], [Domain f]), (solvingInfoL, Maybe (Maybe ([Domain f], [Domain f]), solvingInfoR)))
solveODEIVPBySplittingAtT0End
        solverVT makeMakeInitValFnVec solverVt 
            odeivpG 
    =
    (maybeResult, (infoL, maybeInfoR))
    where
    (maybeResult, maybeInfoR) =
        case maybeResultL of
            Just (valuesLOut, valuesLIn) ->
                case (maybeResultROut, maybeResultRIn) of
                    (Just (resultOut, _), Just (resultInOut, resultInIn)) ->
                        (Just result, Just (Just result, infoROut))
                        where
                        result = (resultOut, makeResultsIn resultInOut resultInIn)
                    (Nothing, _) -> (Nothing, Just (Nothing, infoROut))
                    (_, Nothing) -> (Nothing, Just (Nothing, infoRIn))
                where
                (maybeResultROut, infoROut) = solverVt odeivpROut
                (maybeResultRIn, infoRIn) = solverVt odeivpRIn
                odeivpROut = odeivpR valuesLOut
                odeivpRIn = odeivpR $ mapInconsistentOnes flipConsistency valuesLIn
                makeResultsIn resultInOut resultInIn =
                    map pick $ zip3 whichLValuesConsistent resultInOut resultInIn
                    where
                    pick (True, _, valueIn) = valueIn
                    pick (False, valueOut, _) = flipConsistency valueOut
                mapInconsistentOnes :: (a -> a) -> [a] -> [a]
                mapInconsistentOnes f vec =
                    map fOnConsistent $ zip whichLValuesConsistent vec
                    where
                    fOnConsistent (thisOneIsConsistent, value) 
                        | thisOneIsConsistent = value
                        | otherwise = f value
                whichLValuesConsistent =
                    map (/= Just False) $
                        map (isConsistentEff $ consistencyDefaultEffort sampleDom) valuesLIn
                (sampleDom : _) = valuesLIn
                odeivpR valuesL =
                    odeivpG
                        {
                            odeivp_tStart = odeivp_t0End odeivpG
                        ,
                            odeivp_makeInitialValueFnVec =
                                makeMakeInitValFnVec valuesL
                        }
            _ -> 
                (Nothing, Nothing)
    (maybeResultL, infoL) = solverVT odeivpL
    odeivpL =
        odeivpG
            {
                odeivp_tEnd = odeivp_t0End odeivpG
            }
    
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
    (ODEIVP f -> (Maybe ([Domain f],[Domain f]), solvingInfo)) -- ^ solver to use for segments  
    ->
    ([Domain f] -> ODEInitialValues f) -- ^ how to change initial conditions
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
        Maybe ([Domain f], [Domain f])
    , 
        SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
    )
solveODEIVPBySplittingT
        solver makeMakeInitValFnVec
            effDom splitImprovementThreshold minStepSize 
                odeivpG 
    =
    (valuesAtTEnd, info)
    where
    (valuesAtTEnd, info) = 
        case (splitSolve (replicate dimension False) odeivpG, splitSolve (replicate dimension True) odeivpG) of
            ((Just valuesOut, info2), (Just valuesIn, _)) -> (Just (valuesOut, valuesIn), info2)
            ((_, info2), _) -> (Nothing, info2)
    dimension = length $ odeivp_componentNames odeivpG
    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effDivDomInt = 
        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
    sampleDom = odeivp_tStart odeivpG
    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDom effDom
    
    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
--    sampleImpr = imprecisionOfEff effImpr sampleDom
--    effAddImpr = ArithInOut.fldEffortAdd sampleImpr $ ArithInOut.rrEffortImprecisionField sampleDom effDom
    
    mergeOutIn :: [Bool] -> [a] -> [a] -> [a]
    mergeOutIn shouldRoundInwardsVec vecOut vecIn =
        map pick $ zip3 shouldRoundInwardsVec vecOut vecIn
        where
        pick (True, _, valueIn) = valueIn
        pick (False, valueOut, _) = valueOut
    
    splitSolve shouldRoundInwardsVec odeivp =
--        unsafePrint
--        (
--            "solveODEIVPBySplittingT: splitSolve: "
--            ++ "shouldRoundInwardsVec = " ++ show shouldRoundInwardsVec
--            ++ "tStart = " ++ show tStart
--            ++ "tEnd = " ++ show tEnd
--        ) $
        result
        where
        result
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
                Just (resultOut, resultIn) ->
                    (Just $ mergeOutIn shouldRoundInwardsVec resultOut resultIn, 
                     SegNoSplit directInfo)
                _ -> (Nothing, SegNoSplit directInfo) 
        (maybeDirectResult, directInfo) = solver odeivp
        directComputationFailed =
            case maybeDirectResult of Just _ -> False; _ -> True
        
        splitOnceComputation = -- needed only to decide whether splitting is benefitial, the result is then discarded
            case solver odeivpL of
                (Just (midValuesOut, midValuesIn), _) ->
                    case solver odeivpR of
                        (Just (endValuesOut, endValuesIn), _) ->
                            Just $ mergeOutIn shouldRoundInwardsVec endValuesOut endValuesIn 
                        _ -> Nothing
                    where
                    midValues =
                        mergeOutIn shouldRoundInwardsVec midValuesOut midValuesIn
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
            case splitSolve shouldRoundInwardsVec odeivpL of
                (Just midValues, infoL) -> 
                    case splitSolve shouldRoundInwardsVecConsistent odeivpR of
                        (Nothing, infoR) ->
                            (Nothing, SegSplit (directInfo, maybeSplitImprovement) infoL infoR)
                        (Just endValues, infoR) ->
                            (
                                Just $ mapInconsistentOnes flipConsistency endValues
                            , 
                                SegSplit (directInfo, maybeSplitImprovement) infoL infoR
                            )
                    where
                    mapInconsistentOnes :: (a -> a) -> [a] -> [a]
                    mapInconsistentOnes f vec =
                        map fOnConsistent $ zip whichMidValuesConsistent vec
                        where
                        fOnConsistent (thisOneIsConsistent, value) 
                            | thisOneIsConsistent = value
                            | otherwise = f value
                        whichMidValuesConsistent =
                            map (/= Just False) $
                                map (isConsistentEff $ consistencyDefaultEffort sampleDom) midValues
                    shouldRoundInwardsVecConsistent =
                        mapInconsistentOnes not shouldRoundInwardsVec
                    odeivpR =
                        odeivp
                        {
                            odeivp_tStart = tMid,
                            odeivp_t0End = tMid, -- exact initial time
                            odeivp_makeInitialValueFnVec =
                                makeMakeInitValFnVec $ mapInconsistentOnes flipConsistency midValues
                        }
                failedLeftComputation -> failedLeftComputation
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
        measureImprovementVec vec1 vec2 =
            do
            improvements <- sequence $ 
                                map measureImprovement $ 
                                    zipWith switchDirIfInwards shouldRoundInwardsVec $ 
                                        zip vec1 vec2
            Just $ foldl1 (NumOrd.minOutEff effMinmax) improvements
        switchDirIfInwards True (a,b) = (b,a)
        switchDirIfInwards False pair = pair
        measureImprovement (encl1, encl2) =
            let ?addInOutEffort = effAddDom in
            let ?pCompareEffort = effRefComp in
            do
            refines <- encl1 |<=? encl2
            case refines of
                True -> 
                    Just $ (imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)
                False -> Nothing 
                
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
     Show f, Show (Domain f))
    =>
    (ODEIVP f -> (Maybe ([Domain f], [Domain f]), solvingInfo)) -- ^ solver to use for segments  
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
        Maybe ([Domain f], [Domain f])
    , 
        SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
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
        
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((t0End <-> tStart) >? minStepSize) /= Just True

        directComputation =
            (maybeDirectResult, SegNoSplit directInfo)
        (maybeDirectResult, directInfo) = solver odeivp
        directComputationFailed =
            case maybeDirectResult of Just _ -> False; _ -> True
        
        splitOnceComputation = -- needed only to decide whether splitting is benefitial, the result is then discarded
            case solver odeivpL of
                (Just (endValuesLOut, endValuesLIn), _) -> 
                    case solver odeivpR of
                        (Just (endValuesROut, endValuesRIn), _) ->
                            Just endValues
                            where
                            endValues = (endValuesOut, endValuesIn)
                            endValuesOut =
                                let ?joinmeetEffort = effJoinDom in
                                zipWith (</\>) endValuesLOut endValuesROut
                            endValuesIn =
                                let ?joinmeetEffort = effJoinDom in
                                zipWith (>/\<) endValuesLIn endValuesRIn
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
                (Just (endValuesLOut, endValuesLIn), infoL) -> 
                    case splitSolve odeivpR of
                        (Just (endValuesROut, endValuesRIn), infoR) ->
                            (Just endValues, SegSplit (directInfo, maybeSplitImprovement) infoL infoR)
                            where
                            endValues = (endValuesOut, endValuesIn)
                            endValuesOut =
                                let ?joinmeetEffort = effJoinDom in
                                zipWith (</\>) endValuesLOut endValuesROut
                            endValuesIn =
                                let ?joinmeetEffort = effJoinDom in
                                zipWith (>/\<) endValuesLIn endValuesRIn
                        (Nothing, infoR) ->
                            (Nothing, SegSplit (directInfo, maybeSplitImprovement) infoL infoR)
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
                ((Just (directResult, _), _), Just (splitOnceResult, _)) ->
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
            refines <- encl1 |<=? encl2
            case refines of
                True -> 
                    Just $ (imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)
                False -> Nothing 

data SplittingInfo segInfo splitReason
    = SegNoSplit segInfo
    | SegSplit splitReason (SplittingInfo segInfo splitReason) (SplittingInfo segInfo splitReason)

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
            SegSplit reason infoL infoR ->
                (showSplitReason indent reason)
                ++ "\n" ++
                (shLevel (indent ++ "| ") infoL)
                ++ "\n" ++
                (shLevel (indent ++ "  ") infoR)
             
    