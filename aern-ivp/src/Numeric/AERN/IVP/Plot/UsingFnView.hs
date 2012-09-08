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

module Numeric.AERN.IVP.Plot.UsingFnView where

import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Specification.Hybrid

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding (dblToReal)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.ExactOps

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort


import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent.STM


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
    -> Var f
    -> ODEIVP f
    -> BisectionInfo (Maybe ([f],[f]), t) splitReason
    -> IO ()
plotODEIVPBisectionEnclosures effCF plotMinSegSize tVar ivp bisectionInfo =
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
        toDom = dblToReal sampleCf
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
                    newConstFn sizeLimitsNew domboxNew range
                    where
                    domboxNew = fromList [(tVar, aggrDom)]
                    sizeLimitsNew =
                        adjustSizeLimitsToVarsAndDombox fn [tVar] domboxNew sizeLimits
                    range = evalAtPointOutEff effEval dombox fn
                    sizeLimits = getSizeLimits fn         
                    dombox = getDomainBox fn
                    
            aggrDom = RefOrd.fromEndpointsOutWithDefaultEffort (tStartFirstSeg, tEndLastAggrSeg) 
            (tStartFirstSeg, _) = getTVarDomEndpoints fn1FirstSeg
            (_, tEndLastAggrSeg) = getTVarDomEndpoints fn1LastAggrSeg
            (segNoLastAggrSeg, ((fn1LastAggrSeg,_) : _)) = last smallSegmentsToAggregate 
            getTVarDomEndpoints fn =
                case lookupVar (getDomainBox fn) tVar of 
                    Just tDom -> RefOrd.getEndpointsOutWithDefaultEffort tDom 
            (fnsFirstSeg, fnNamesFirstSeg) = unzip fnsNamesFirstSeg
        aggrNewSegm prevFns prevFnNames prevSegNames _ =
            (reverse prevFns, reverse prevFnNames, reverse prevSegNames)
    whichActive list =
        take (length list) activityCycle 
        where
        activityCycle = cycle $ map snd $ zip componentNames $ 
            True : (repeat True) 
--            True : (repeat False) 
--            True : False : False : True : (repeat False) 
--            True : False : False : False : True : (repeat False) 
    
    giveColours list =
        take (length list) colourCycle
        where
        colourCycle = cycle $ map snd $ 
            zip componentNames 
                (cycle [blue, green, red, black])
--                (cycle [black]) 

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
