{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.IVP.Plot.UsingFnView
    Description :  visualising IVP solutions using FnView 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Visualising IVP solutions using FnView.
-}

module Numeric.AERN.IVP.Plot.UsingFnView 
(
    plotODEIVPBisectionEnclosures,
    plotHybIVPBisectionEnclosures,
    plotHybIVPListEnclosures
)
where

import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Events.EventTree
import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Specification.Hybrid

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding (dblToReal)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

import Numeric.AERN.RealArithmetic.ExactOps

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.Operators

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

import Numeric.AERN.Basics.SizeLimits

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent.STM

import qualified Data.Map as Map

import Numeric.AERN.Misc.Debug
_ = unsafePrint

plotODEIVPBisectionEnclosures :: 
    (Var f ~ String,
     CanEvaluate f,
     CairoDrawableFn f,
     HasSizeLimits f,
     RefOrd.RoundedLattice f,
     HasConstFns f,
     HasDomainBox f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show f, Show (Var f), Show (Domain f)
    ) 
    =>
    ArithInOut.RoundedRealEffortIndicator (Domain f)
    -> Domain f
    -> ODEIVP f
    -> BisectionInfo (Maybe ([f],[f]), t) splitReason
    -> IO ()
plotODEIVPBisectionEnclosures effCF plotMinSegSize ivp bisectionInfo =
    do
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    Gtk.mainGUI
    where
    effDrawFn = cairoDrawFnDefaultEffort sampleFn
    effEval = evaluationDefaultEffort sampleFn
    ((sampleFn : _) : _) = fns
    sampleCf = getSampleDomValue sampleFn
    
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp
    tEnd = odeivp_tEnd ivp
    tVar = odeivp_tVar ivp
    
    addPlotVar = map $ map addV
        where
        addV fn = (fn, tVar)
    (fns, fnNames, segNames) = 
        aggregateSequencesOfTinySegments fnsAndNames 
    fnsAndNames = 
        map getFnsFromSegInfo $
            bisectionInfoGetLeafSegInfoSequence bisectionInfo
        where
        getFnsFromSegInfo (Just (fnVec, _), _) =
            zip fnVec componentNames
        getFnsFromSegInfo _ = []
    fnmeta = 
        (FV.defaultFnMetaData sampleFn)
        {
            FV.dataFnGroupNames = segNames, -- map ("segment " ++) (map show [1..segs]),
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map giveColours fnNames,
            FV.dataDomL = tStart,
            FV.dataDomR = tEnd,
            FV.dataValLO = neg domainHalf,
            FV.dataValHI = domainHalf,
            FV.dataDomName = "t",
            FV.dataDefaultActiveFns = map whichActive fnNames,
            FV.dataDefaultEvalPoint = tEnd,
            FV.dataDefaultCanvasParams =
                (FV.defaultCanvasParams sampleCf)
                {
                    FV.cnvprmCoordSystem = 
                        FV.CoordSystemLinear $ 
                            FV.Rectangle  domainHalf (neg domainHalf) tStart tEnd
                    ,
                    FV.cnvprmSamplesPerUnit = 200
                    ,
                    FV.cnvprmBackgroundColour = Just (1,1,1,1)
                }
        }
        where
        domainHalf = (tEnd <-> tStart) </>| (2 :: Double)
    aggregateSequencesOfTinySegments fnsAndNames2 = 
        aggrNewSegm [] [] [] $ zip ([1..]::[Int]) fnsAndNames2
        where
        aggrNewSegm 
                prevFns prevFnNames prevSegNames 
                segs@((segNoFirstSeg, fnsNamesFirstSeg@((fn1FirstSeg,_) : _)) : restSegs)
            | noAggregation =
                aggrNewSegm 
                    (fnsFirstSeg : prevFns) 
                    (fnNamesFirstSeg : prevFnNames) 
                    (("segment " ++ show segNoFirstSeg) : prevSegNames) 
                    restSegs 
            | otherwise =
                aggrNewSegm
                    (fnsAggregated : prevFns) 
                    (fnNamesAggregated : prevFnNames) 
                    (("segments " ++ show segNoFirstSeg ++ "-" ++ show segNoLastAggrSeg) : prevSegNames) 
                    restSegsAfterAggr
            where
            noAggregation = length smallSegmentsToAggregate <= 1
            (smallSegmentsToAggregate, restSegsAfterAggr) = 
                span segEndsBeforeLimit segs
                where
                segEndsBeforeLimit (_, ((fn1ThisSeg,_) : _)) =
                    (tEndThisSeg <=? tAggrLimit) == Just True
                    where
                    (_, tEndThisSeg) = getTVarDomEndpoints fn1ThisSeg
                    tAggrLimit = tStartFirstSeg <+> plotMinSegSize
            fnNamesAggregated =
                map (++ "(aggr)") componentNames
            fnsAggregated =
                foldl1 (zipWith (</\>)) $
                    chunksOf (length componentNames) $
                        map makeConstFnOverAggrDom $
                            concat $ map getFnsFromSeg smallSegmentsToAggregate
                where
                chunksOf _ [] = []
                chunksOf n list = firstN : (chunksOf n rest)
                    where
                    (firstN, rest) = splitAt n list
                getFnsFromSeg (_, fnsNames) = map fst fnsNames
                makeConstFnOverAggrDom fn =
                    newConstFn sizeLimits [(tVar, aggrDom)] range
                    where
                    range = evalAtPointOutEff effEval dombox fn
                    sizeLimits = getSizeLimits fn         
                    dombox = getDomainBox fn
                    
            aggrDom = RefOrd.fromEndpointsOut (tStartFirstSeg, tEndLastAggrSeg) 
            (tStartFirstSeg, _) = getTVarDomEndpoints fn1FirstSeg
            (_, tEndLastAggrSeg) = getTVarDomEndpoints fn1LastAggrSeg
            (segNoLastAggrSeg, ((fn1LastAggrSeg,_) : _)) = last smallSegmentsToAggregate 
            getTVarDomEndpoints fn =
                case lookupVar (getDomainBox fn) tVar of 
                    Just tDom -> RefOrd.getEndpointsOut tDom 
            (fnsFirstSeg, fnNamesFirstSeg) = unzip fnsNamesFirstSeg
        aggrNewSegm prevFns prevFnNames prevSegNames _ =
            (reverse prevFns, reverse prevFnNames, reverse prevSegNames)
    whichActive list =
        take (length list) activityCycle 
        where
        activityCycle = cycle $ map snd $ zip componentNames $ 
--            True : (repeat True) 
            (concat $ repeat [True, False]) 
--            True : (repeat False) 
--            True : False : False : True : (repeat False) 
--            True : False : False : False : True : (repeat False) 
    
    giveColours list =
        take (length list) colourCycle
        where
        colourCycle = cycle $ map snd $ 
            zip componentNames 
                (cycle [blue, green, red])
--                (cycle [black]) 

plotHybIVPBisectionEnclosures :: 
    (Var f ~ String,
     CanEvaluate f,
     CairoDrawableFn f,
     HasSizeLimits f,
     RefOrd.RoundedLattice f,
     HasConstFns f,
     HasDomainBox f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show f, Show (Var f), Show (Domain f)
    ) 
    =>
    ArithInOut.RoundedRealEffortIndicator (Domain f)
    -> Bool 
    -> Domain f
    -> HybridIVP f
    -> BisectionInfo (t, t1, [(HybSysMode, EventInfo f)]) splitReason
    -> IO ()
plotHybIVPBisectionEnclosures effCF shouldShowEventTreeEnclosures plotMinSegSize ivp bisectionInfo =
    do
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    Gtk.mainGUI
    where
    effDrawFn = cairoDrawFnDefaultEffort sampleFn
    effEval = evaluationDefaultEffort sampleFn
    ((sampleFn : _) : _) = fns 
    sampleCf = getSampleDomValue sampleFn
    
    componentNames = hybsys_componentNames $ hybivp_system ivp
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp
    
    addPlotVar = map $ map addV
        where
        addV fn = (fn, tVar)
    (fns, fnNames, segNames) = 
        aggregateSequencesOfTinySegments effEval componentNames tVar plotMinSegSize fnsAndNames 
    fnsAndNames =
        concat $ map maybeAggregateSegment fnAndNameVectors
        where
        maybeAggregateSegment fnAndNameVectorsInSegment
            | shouldShowEventTreeEnclosures = fnAndNameVectorsInSegment
            | otherwise = 
                case fnAndNameVectorsInSegment of
                    (_ : _ : _) -> [numberFnVec unionAllVectors ""]
                    _ -> fnAndNameVectorsInSegment
            where
            unionAllVectors =
--                unsafePrint (
--                    "unionAllVectors:"
--                    ++ "\n nameVectorsInSegment = " ++ show nameVectorsInSegment 
--                    ++ "\n fnVectorsInSegment = " ++ show fnVectorsInSegment 
--                    ++ "\n rangeVectors = " ++ 
--                        (concat $ map ("\n    " ++) (map show $ concat rangeVectors)) 
--                    ++ "\n unifiedRangeVector = " ++ show unifiedRangeVector 
--                )$
                result
                where
                result = map (newConstFnFromSample sampleFnInSegment) unifiedRangeVector
                unifiedRangeVector = foldl1 (zipWith (</\>)) rangeVectors 
                rangeVectors = map (map getRange) fnVectorsInSegment
                fnVectorsInSegment@((sampleFnInSegment : _) : _) = map (map fst) fnAndNameVectorsInSegment
                nameVectorsInSegment = map (map snd) fnAndNameVectorsInSegment
                getRange fn =
                    evalAtPointOutEff effEval2 dombox fn
                    where
                    dombox = getDomainBox fn
                effEval2 = evaluationDefaultEffort sampleFnInSegment
        fnAndNameVectors =
            map getFnsFromSegInfo $ bisectionInfoGetLeafSegInfoSequence bisectionInfo
        getFnsFromSegInfo (_,_,modeEventInfos) =
            concat $ map getFnsFromMEI modeEventInfos
        getFnsFromMEI (HybSysMode modeName, eventInfo) =
            collectFns modeName eventInfo
        collectFns namePrefix (EventNextSure (_, fnVec) eventMap) =
            [numberFnVec fnVec (namePrefix ++ ".")] ++
            (concat $ map perEvent $ toAscList eventMap)
            where
            perEvent (HybSysEventKind eventName, subEventInfo) =
                collectFns (namePrefix ++ "!" ++ eventName) subEventInfo
        collectFns namePrefix (EventNextMaybe (_, fnVec) eventMap) =
            [numberFnVec fnVec (namePrefix ++ ".")] ++
            (concat $ map perEvent $ toAscList eventMap)
            where
            perEvent (HybSysEventKind eventName, subEventInfo) =
                collectFns (namePrefix ++ "?" ++ eventName) subEventInfo
        collectFns namePrefix (EventFixedPoint (_, fnVec)) =
            [numberFnVec fnVec (namePrefix ++ ".")]
        collectFns _ _ = 
            []
        numberFnVec fnVec namePrefix =
            zipWith addName fnVec componentNames
            where
            addName fn compName = (fn, namePrefix ++ compName)
    fnmeta = 
        (FV.defaultFnMetaData sampleFn)
        {
            FV.dataFnGroupNames = segNames, -- map ("segment " ++) (map show [1..segs]),
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map giveColours fnNames,
            FV.dataDomName = "t",
            FV.dataDomL = tStart,
            FV.dataDomR = tEnd,
            FV.dataValLO = neg domainHalf,
            FV.dataValHI = domainHalf,
            FV.dataDefaultActiveFns = map whichActive fnNames,
            FV.dataDefaultEvalPoint = tEnd,
            FV.dataDefaultCanvasParams =
                (FV.defaultCanvasParams sampleCf)
                {
                    FV.cnvprmCoordSystem = 
                        FV.CoordSystemLinear $ 
                            FV.Rectangle  domainHalf (neg domainHalf) tStart tEnd
                    ,
                    FV.cnvprmSamplesPerUnit = 200
                    ,
                    FV.cnvprmBackgroundColour = Just (1,1,1,1)
                }
        }
        where
        domainHalf = (tEnd <-> tStart) </>| (2 :: Double)
    whichActive list =
        take (length list) activityCycle 
        where
        activityCycle = cycle $ map snd $ zip componentNames $ 
--            True : (repeat True) 
            True : (repeat False) 
--            True : False : False : True : (repeat False) 
--            True : False : False : False : True : (repeat False) 
    
    giveColours list =
        take (length list) colourCycle
        where
        colourCycle = cycle $ map snd $ 
            zip componentNames 
--                (cycle [blue, green, red, black])
                (cycle [black]) 


aggregateSequencesOfTinySegments effEval componentNames tVar plotMinSegSize fnsAndNames2 
    = aggrNewSegm [] [] [] $ zip ([1..]::[Int]) fnsAndNames2
    where
    aggrNewSegm 
            prevFns prevFnNames prevSegNames 
            segs@((segNoFirstSeg, fnsNamesFirstSeg@((fn1FirstSeg,_) : _)) : restSegs)
        | noAggregation =
            aggrNewSegm 
                (fnsFirstSeg : prevFns) 
                (fnNamesFirstSeg : prevFnNames) 
                (("segment " ++ show segNoFirstSeg) : prevSegNames) 
                restSegs 
        | otherwise =
            aggrNewSegm
                (fnsAggregated : prevFns) 
                (fnNamesAggregated : prevFnNames) 
                (("segments " ++ show segNoFirstSeg ++ "-" ++ show segNoLastAggrSeg) : prevSegNames) 
                restSegsAfterAggr
        where
        noAggregation = length smallSegmentsToAggregate <= 1
        (smallSegmentsToAggregate, restSegsAfterAggr) = 
            span segEndsBeforeLimit segs
            where
            segEndsBeforeLimit (_, ((fn1ThisSeg,_) : _)) =
                (tEndThisSeg <=? tAggrLimit) == Just True
                where
                (_, tEndThisSeg) = getTVarDomEndpoints fn1ThisSeg
                tAggrLimit = tStartFirstSeg <+> plotMinSegSize
        fnNamesAggregated =
            map (++ "(aggr)") componentNames
        fnsAggregated =
            foldl1 (zipWith (</\>)) $
                chunksOf (length componentNames) $
                    map makeConstFnOverAggrDom $
                        concat $ map getFnsFromSeg smallSegmentsToAggregate
            where
            chunksOf _ [] = []
            chunksOf n list = firstN : (chunksOf n rest)
                where
                (firstN, rest) = splitAt n list
            getFnsFromSeg (_, fnsNames) = map fst fnsNames
            makeConstFnOverAggrDom fn =
                newConstFn sizeLimits [(tVar, aggrDom)] range
                where
                sizeLimits = getSizeLimits fn         
                range = evalAtPointOutEff effEval dombox fn
                dombox = getDomainBox fn
                
        aggrDom = RefOrd.fromEndpointsOut (tStartFirstSeg, tEndLastAggrSeg) 
        (tStartFirstSeg, _) = getTVarDomEndpoints fn1FirstSeg
        (_, tEndLastAggrSeg) = getTVarDomEndpoints fn1LastAggrSeg
        (segNoLastAggrSeg, ((fn1LastAggrSeg,_) : _)) = last smallSegmentsToAggregate 
        getTVarDomEndpoints fn =
            case lookupVar (getDomainBox fn) tVar of 
                Just tDom -> RefOrd.getEndpointsOut tDom 
        (fnsFirstSeg, fnNamesFirstSeg) = unzip fnsNamesFirstSeg
    aggrNewSegm prevFns prevFnNames prevSegNames _ =
        (reverse prevFns, reverse prevFnNames, reverse prevSegNames)


plotHybIVPListEnclosures :: 
    (Var f ~ String,
     CanEvaluate f,
     CairoDrawableFn f,
     HasSizeLimits f,
     RefOrd.RoundedLattice f,
     HasConstFns f,
     HasDomainBox f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show f, Show (Var f), Show (Domain f),
     solvingInfoODESegment ~ (Maybe ([f],[f]), (Domain f, Maybe [Domain f])),
     solvingInfoODE ~ BisectionInfo solvingInfoODESegment (solvingInfoODESegment, Maybe (Domain f)),
     solvingInfoEvents ~ (Domain f, Maybe (HybridSystemUncertainState (Domain f)), EventInfo f)
     ) 
    =>
    ArithInOut.RoundedRealEffortIndicator (Domain f)
    -> 
    Domain f
    -> 
    HybridIVP f
    -> 
    [(
        Domain f
        -- end time of this segment (including the event resolution sub-segment)  
     ,
        Maybe (HybridSystemUncertainState (Domain f))
     ,
        Map.Map HybSysMode 
            (
                solvingInfoODE,
                Maybe (HybridSystemUncertainState (Domain f)),
                Maybe solvingInfoEvents
            )
     )
    ]
    -> 
    IO ()
plotHybIVPListEnclosures effCF _plotMinSegSize ivp segmentsInfo =
    do
    Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    Gtk.mainGUI
    where
    effDrawFn = cairoDrawFnDefaultEffort sampleFn
    effEval = evaluationDefaultEffort sampleFn
    ((sampleFn : _) : _) = fns 
    sampleCf = getSampleDomValue sampleFn
    
    componentNames = hybsys_componentNames $ hybivp_system ivp
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp
    
    addPlotVar = map $ map addV
        where
        addV fn = (fn, tVar)
    
    (fns, fnNames, groupNames) = 
        unzip3 $
            map getFnsFromSegModeInfo $ 
                concat $ map getSegModeInfo
                    $ zip ([1..]::[Int]) segmentsInfo
        where
        getSegModeInfo (segNo, (_, _, modeSolvingInfoMap)) =
            map (\i -> (segNo,i)) $ Map.toList modeSolvingInfoMap
        getFnsFromSegModeInfo (segNo, (HybSysMode modeName, (bisectionInfo, _, maybeEventSolvingInfo))) =
            (groupFns, groupFnNames, groupName)
            where
            groupName = "seg" ++ show segNo ++ "." ++ modeName
            (groupFns, groupFnNames) =
                unzip $ fnsAndNames
            fnsAndNames = 
                fnsAndNamesNoEvents ++ 
                fnsAndNamesEvents
            fnsAndNamesNoEvents =
                concat $
                    map getFnsFromSegInfo $
                        zip ([1..]::[Int]) $
                            bisectionInfoGetLeafSegInfoSequence bisectionInfo
                where
                getFnsFromSegInfo (n, (Just (fnVec, _), _)) =
                    nameFnVec fnVec prefix
                    where
                    prefix = "noev" ++ (show n)
                getFnsFromSegInfo _ = []
            fnsAndNamesEvents =
                case maybeEventSolvingInfo of
                    Nothing -> []
                    Just (_,_,eventInfo) ->
                        getFnsFromEventInfo eventInfo
            getFnsFromEventInfo eventInfo =
                collectFns "" eventInfo
            collectFns namePrefix (EventNextSure (_, fnVec) eventMap) =
                (nameFnVec fnVec namePrefix) ++
                (concat $ map perEvent $ toAscList eventMap)
                where
                perEvent (HybSysEventKind eventName, subEventInfo) =
                    collectFns (namePrefix ++ "!" ++ eventName) subEventInfo
            collectFns namePrefix (EventNextMaybe (_, fnVec) eventMap) =
                (nameFnVec fnVec namePrefix) ++
                (concat $ map perEvent $ toAscList eventMap)
                where
                perEvent (HybSysEventKind eventName, subEventInfo) =
                    collectFns (namePrefix ++ "?" ++ eventName) subEventInfo
            collectFns namePrefix (EventFixedPoint (_, fnVec)) =
                (nameFnVec fnVec namePrefix)
            collectFns _ _ = 
                []
            nameFnVec fnVec namePrefix =
                zipWith addName fnVec componentNames
                where
                addName fn compName = (fn, namePrefix ++ "." ++ compName)
    fnmeta = 
        (FV.defaultFnMetaData sampleFn)
        {
            FV.dataFnGroupNames = groupNames,
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map giveColours fnNames,
            FV.dataDomName = tVar,
            FV.dataDomL = tStart,
            FV.dataDomR = tEnd,
            FV.dataValLO = neg domainHalf,
            FV.dataValHI = domainHalf,
            FV.dataDefaultActiveFns = map whichActive fnNames,
            FV.dataDefaultEvalPoint = tEnd,
            FV.dataDefaultCanvasParams =
                (FV.defaultCanvasParams sampleCf)
                {
                    FV.cnvprmCoordSystem = 
                        FV.CoordSystemLinear $ 
                            FV.Rectangle  domainHalf (neg domainHalf) tStart tEnd
                    ,
                    FV.cnvprmSamplesPerUnit = 200
                    ,
                    FV.cnvprmBackgroundColour = Just (1,1,1,1)
                }
        }
        where
        domainHalf = (tEnd <-> tStart) </>| (2 :: Double)
    whichActive list =
        take (length list) activityCycle 
        where
        activityCycle = cycle $ map snd $ zip componentNames $ 
--            True : (repeat True)
            True : (repeat False) 
--            True : False : False : True : (repeat False) 
--            True : False : False : False : True : (repeat False) 
    
    giveColours list =
        take (length list) colourCycle
        where
        colourCycle = cycle $ map snd $ 
            zip componentNames 
--                (cycle [blue, green, red, black])
                (cycle [black])

black = FV.defaultFnPlotStyle
blue = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
        FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
    } 
green = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.8,0.1,1), 
        FV.styleFillColour = Just (0.1,0.8,0.1,0.1) 
    } 
red = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.8,0.1,0.1,1), 
        FV.styleFillColour = Just (0.8,0.1,0.1,0.1) 
    } 
        