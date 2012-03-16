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
    solveBySplitting,
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

--import Numeric.AERN.Misc.Debug
        

solveBySplitting ::
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
    (ODEIVP f -> (Maybe [Domain f], solvingInfo)) -- ^ solver to use for segments  
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
        Maybe [Domain f]
    , 
        SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
    )
solveBySplitting 
        solver makeMakeInitValFnVec
            effDom splitImprovementThreshold minStepSize 
                odeivpG 
    =
    ssolve odeivpG
    where
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
    
    ssolve odeivp
        | belowStepSize = directComputation
        | otherwise = 
            case maybeSplitImprovement of
                Just improvementBy 
                    | (improvementBy >? splitImprovementThreshold) == Just True -> 
                        splitComputation
                _ -> directComputation
        where
        tStart = odeivp_tStart odeivp
        tEnd = odeivp_tEnd odeivp
        
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((tEnd <-> tStart) >? minStepSize) /= Just True

        directComputation =
            (maybeDirectResult, SegNoSplit directInfo)
        (maybeDirectResult, directInfo) = solver odeivp
        
        splitComputation =
            case ssolve odeivpL of
                (Just midValues, infoL) -> 
                    case ssolve odeivpR of
                        (Just endValues, infoR) ->
                            (Just endValues, SegSplit (directInfo, maybeSplitImprovement) infoL infoR)
                        (Nothing, infoR) ->
                            (Nothing, SegSplit (directInfo, maybeSplitImprovement) infoL infoR)
                    where
                    odeivpR =
                        odeivp
                        {
                            odeivp_tStart = tMid,
                            odeivp_t0End = tMid, -- exact initial time
                            odeivp_makeInitialValueFnVec =
                                makeMakeInitValFnVec midValues 
                        }
                failedLeftComputation -> failedLeftComputation
            where
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
            case (directComputation, splitComputation) of
                ((Just directResult, _), (Just splitResult, _)) ->
                    measureImprovementVec directResult splitResult
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
    (segInfo -> String) 
    -> 
    (splitReason -> String) 
    -> 
    SplittingInfo segInfo splitReason -> String
showSplittingInfo showSegInfo showSplitReason splittingInfoG =
    shLevel "" splittingInfoG
    where
    shLevel indent splittingInfo =
        case splittingInfo of
            SegNoSplit segInfo -> indent ++ showSegInfo segInfo
            SegSplit reason infoL infoR ->
                (indent ++ showSplitReason reason)
                ++ "\n" ++
                (shLevel (indent ++ "| ") infoL)
                ++ "\n" ++
                (shLevel (indent ++ "  ") infoR)
             
    