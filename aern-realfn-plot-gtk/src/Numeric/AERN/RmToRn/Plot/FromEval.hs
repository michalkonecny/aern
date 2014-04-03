{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.FromEval
    Description :  plotting on gtk canvas using evaluation at some points
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Plotting on gtk canvas using evaluation at some points.
-}

module Numeric.AERN.RmToRn.Plot.FromEval where

import Numeric.AERN.RmToRn.Plot.Params
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.NumericOrder as NumOrd

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RmToRn.Interval

import Numeric.AERN.Misc.Debug

import Graphics.Rendering.Cairo

import qualified Data.Map as Map
--import qualified Data.List as List

instance 
    (HasAntiConsistency (Domain f),
     CanEvaluate f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f), Show (Var f), Show (VarBox f (Domain f))
    )
    =>
    (CairoDrawableFn (Interval f))
    where
    type CairoDrawFnEffortIndicator (Interval f) =
        CairoDrawEffortIndicatorFnFromEval (Interval f)
    cairoDrawFnDefaultEffort = cairoDrawFnDefaultEffortFromEval
    cairoDrawFnGraph = cairoDrawFnGraphFromEval
    cairoDrawFnParameteric = cairoDrawFnParametericFromEval
    
type CairoDrawEffortIndicatorFnFromEval f =
    (
     EvaluationEffortIndicator f
    ,
    ( 
     ArithInOut.RoundedRealEffortIndicator (Domain f)
     ,
     RefOrd.GetEndpointsEffortIndicator (Domain f)
     ,
     ConsistencyEffortIndicator (Domain f)
     )
    )

cairoDrawFnDefaultEffortFromEval ::
    (HasAntiConsistency (Domain f),
     CanEvaluate f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    =>
    f ->
    CairoDrawEffortIndicatorFnFromEval f
cairoDrawFnDefaultEffortFromEval sampleF =
    (
       evaluationDefaultEffort sampleF
       ,
       (
        ArithInOut.roundedRealDefaultEffort sampleDF
        ,
        RefOrd.getEndpointsDefaultEffort sampleDF
        ,
        consistencyDefaultEffort sampleDF
       )
    )
    where
    sampleDF = getSampleDomValue sampleF

cairoDrawFnGraphFromEval ::
    (HasAntiConsistency (Domain f),
     CanEvaluate f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f), Show (Var f), Show (VarBox f (Domain f))
    )
    =>
    CairoDrawEffortIndicatorFnFromEval f ->
    CanvasParams (Domain f) ->
    ((Domain f, Domain f) -> (Double, Double)) ->
    FnPlotStyle ->
    Var f ->
    f ->
    Render ()
cairoDrawFnGraphFromEval 
        eff@(effEval, (effReal, effGetE, effConsistency))
        canvasParams toScreenCoords 
        style plotVar fn 
    = 
--    unsafePrint (
--        "cairoDrawFnFromEval: starting"
--        ++ "\n dom = " ++ show dom
--        ++ "\n canvasParams = " ++ show canvasParams
--    ) $ 
    case isAntiConsistentEff effConsistency visibleDom of
        Just False ->
--            unsafePrint (
--                "cairoDrawFnFromEval: visible domain not anti-consistent"
--                ++ "\n partition = " ++ show partition
--            ) $ 
            plotEnclosure
            where
            plotEnclosure =
                do
                -- fill the enclosure first:
                case styleFillColour style of
                    Just (r,g,b,a) ->
                        do 
                        setSourceRGBA r g b a
                        enclosureOutline
                        fill
                    _ -> return ()
                -- then draw the outline:
                case styleOutlineColour style of
                    Just (r,g,b,a) ->
                        do 
                        setSourceRGBA r g b a
                        enclosureOutline
                        setLineWidth $ styleOutlineThickness style
                        stroke
                    _ -> return ()
                where
                enclosureOutline =
                    do
                    moveToPt firstPointUp
                    mapM_ lineToPt pointsDn
                    mapM_ lineToPt (reverse pointsUp)
                (firstPointUp : _) = pointsUp
                pointsUp = zip partition enclosureValuesUp
                pointsDn = zip partition enclosureValuesDn
                (enclosureValuesDn, enclosureValuesUp) =
                    unzip $ map (RefOrd.getEndpointsOutEff effGetE) enclosureValues
                moveToPt = usePt moveTo 
                lineToPt = usePt lineTo 
                usePt fn pt = fn xD yD
                    where
                    (xC,yC) = translateToCoordSystem effReal coordSystem pt
                    (xD, yD) = toScreenCoords (xC,yC)
            enclosureValues =
                map evalPt $ map mkPt partition
            mkPt d =
                insertVar plotVar d dombox
            evalPt pt =
                evalAtPointOutEff effEval pt fn
            partition =
                [domLO] ++ (map ithPt [1..(segCnt -1)]) ++ [domHI]
                where
                (domLO, domHI) = 
                    RefOrd.getEndpointsOutEff effGetE visibleDom
                ithPt i =
                    ((domLO <*>| (segCnt - i)) <+> (domHI <*>| i)) </>| segCnt
                segCnt =
                    getSegmentCount segPerUnit coordSystem visibleDom 
                        -- in hsreals this was dom instead of visibleDom 
                getSegmentCount segPerUnit coordSystem dom = 
--                        unsafePrint ("cairoDrawFnFromEval: getSegmentCount: dom = " ++ show dom ++ " domWidthScreen = " ++ show domWidthScreen) $
                    case ArithUpDn.convertUpEff effToInt 0 (segPerUnit |<*> domWidthScreen) of
                        Just cnt -> (cnt :: Int)
                    where
                    domWidthScreen = 
                        NumOrd.minOutEff effMinmax c1 $ domHIScreen <-> domLOScreen
                    (domLOScreen, _) = 
                        translateToCoordSystem effReal coordSystem (domLO, c1)
                    (domHIScreen, _) = 
                        translateToCoordSystem effReal coordSystem (domHI, c1)
                    (domLO, domHI) = RefOrd.getEndpointsOutEff effGetE dom
                    c1 = one sampleDF
--            pickActiveVals vals =
--                fst $ unzip $ filter snd $ zip vals activeDimensions
        _ -> return ()
    where
    visibleDom = 
        dom <\/> (vdomLO </\> vdomHI)
        where
        (_,_,vdomLO, vdomHI) = getVisibleDomExtents coordSystem
    dom =
        case lookupVar dombox plotVar of 
            Just dom -> dom
            _ -> error $ 
                "aern-realfn-plot-gtk error: plotVar not present in dombox:"
                ++ "\n  plotVar = " ++ show plotVar 
                ++ "\n  dombox = " ++ show dombox 
    dombox = getDomainBox fn
    coordSystem = cnvprmCoordSystem canvasParams
    segPerUnit = cnvprmSamplesPerUnit canvasParams
--    activeDimensions = cnvprmPlotDimensions canvasParams
    
    (<\/>) = RefOrd.joinOutEff effJoinMeet
    (</\>) = RefOrd.meetOutEff effJoinMeet
    (<+>) = ArithInOut.addOutEff effAdd
    (<->) = ArithInOut.subtrOutEff effAdd
    (<*>|) = ArithInOut.mixedMultOutEff effMultInt
    (|<*>) = flip $ ArithInOut.mixedMultOutEff effMultInt
    (</>|) = ArithInOut.mixedDivOutEff effDivInt
    
    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDF effReal
    effJoinMeet = ArithInOut.rrEffortJoinMeet sampleDF effReal
    effToInt = ArithInOut.rrEffortToInt sampleDF effReal
    effAdd =
        ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effReal
    effMultInt =
        ArithInOut.mxfldEffortMult sampleDF sampleI $ ArithInOut.rrEffortIntMixedField sampleDF effReal
    effDivInt =
        ArithInOut.mxfldEffortDiv sampleDF sampleI $ ArithInOut.rrEffortIntMixedField sampleDF effReal
    
    sampleF = fn
    sampleDF = dom
    sampleI = 1 :: Int
    


cairoDrawFnParametericFromEval ::
    (HasAntiConsistency (Domain f),
     CanEvaluate f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f), Show (Var f), Show (VarBox f (Domain f))
    )
    =>
    CairoDrawEffortIndicatorFnFromEval f ->
    CanvasParams (Domain f) ->
    ((Domain f, Domain f) -> (Double, Double)) ->
    FnPlotStyle ->
    Var f ->
    (f,f) ->
    Render ()
cairoDrawFnParametericFromEval 
        eff@(effEval, (effReal, effGetE, effConsistency))
        canvasParams toScreenCoords 
        style plotVar (fnX, fnY) 
    = 
    plotSampleBoxes
    where
    plotSampleBoxes =
        mapM_ plotValueSample enclosureSamples
    plotValueSample (xI, yI) =
        do
        -- fill the box first:
        case styleFillColour style of
            Just (r,g,b,a) ->
                do
                setSourceRGBA r g b a
                boxOutline
                fill
            _ -> return ()
        -- then draw the box outline:
        case styleOutlineColour style of
            Just (r,g,b,a) ->
                do 
                setSourceRGBA r g b a
                boxOutline
                setLineCap LineCapRound
                setLineWidth $ styleOutlineThickness style
                stroke
            _ -> return ()
        where
        boxOutline =
            do
            moveToPt corner00
            mapM_ lineToPt [corner01, corner11, corner10, corner00]
        corner00 = (x0, y0)
        corner01 = (x0, y1)
        corner10 = (x1, y0)
        corner11 = (x1, y1)
        (x0, x1) = RefOrd.getEndpointsOutEff effGetE xI
        (y0, y1) = RefOrd.getEndpointsOutEff effGetE yI
        moveToPt = usePt moveTo 
        lineToPt = usePt lineTo 
        usePt fn pt = fn xD yD
            where
            (xC,yC) = translateToCoordSystem effReal coordSystem pt
            (xD,yD) = toScreenCoords (xC,yC)
    enclosureSamples =
        map evalPt $ map mkPt partition
    mkPt d =
        insertVar plotVar d dombox
    evalPt pt =
        (evalAtPointOutEff effEval pt fnX, evalAtPointOutEff effEval pt fnY) 
    partition =
        [domLO] ++ (map ithPt [1..(segCnt -1)]) ++ [domHI]
        where
        (domLO, domHI) = 
            RefOrd.getEndpointsOutEff effGetE dom
        ithPt i =
            ((domLO <*>| (segCnt - i)) <+> (domHI <*>| i)) </>| segCnt
        segCnt :: Int
        segCnt = round $ (fromInteger $ toInteger segPerUnit) * domSize
        domSize :: Double
        Just domSize = ArithUpDn.convertUp 0 $ domHI <-> domLO
    dom =
        case lookupVar dombox plotVar of 
            Just dom2 -> dom2
            _ -> error $ 
                "aern-realfn-plot-gtk error: plotVar not present in dombox:"
                ++ "\n  plotVar = " ++ show plotVar 
                ++ "\n  dombox = " ++ show dombox 
    dombox = getDomainBox fnX
    coordSystem = cnvprmCoordSystem canvasParams
    segPerUnit = cnvprmSamplesPerUnit canvasParams
--    activeDimensions = cnvprmPlotDimensions canvasParams
    
    (<->) = ArithInOut.subtrOutEff effAdd
    (<+>) = ArithInOut.addOutEff effAdd
    (<*>|) = ArithInOut.mixedMultOutEff effMultInt
    (</>|) = ArithInOut.mixedDivOutEff effDivInt
    
    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDF effReal
    effJoinMeet = ArithInOut.rrEffortJoinMeet sampleDF effReal
    effToInt = ArithInOut.rrEffortToInt sampleDF effReal
    effAdd =
        ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effReal
    effMultInt =
        ArithInOut.mxfldEffortMult sampleDF sampleI $ ArithInOut.rrEffortIntMixedField sampleDF effReal
    effDivInt =
        ArithInOut.mxfldEffortDiv sampleDF sampleI $ ArithInOut.rrEffortIntMixedField sampleDF effReal
    
    sampleF = fnX
    sampleDF = dom
    sampleI = 1 :: Int
    
    