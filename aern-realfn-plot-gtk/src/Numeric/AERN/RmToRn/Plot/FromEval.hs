{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
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
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort

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
    cairoDrawFn = cairoDrawFnFromEval
    
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

cairoDrawFnFromEval ::
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
cairoDrawFnFromEval 
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
                    let ?addInOutEffort = effAdd in
                    let ?mixedMultInOutEffort = effMultInt in
                    let ?mixedDivInOutEffort = effDivInt in
                    ((domLO <*>| (segCnt - i)) <+> (domHI <*>| i)) </>| segCnt
                segCnt =
                    let ?addInOutEffort = effAdd in
                    let ?mixedMultInOutEffort = effMultInt in
                    getSegmentCount segPerUnit coordSystem visibleDom 
                        -- in hsreals this was dom instead of visibleDom 
                getSegmentCount segPerUnit coordSystem dom = 
--                        unsafePrint ("cairoDrawFnFromEval: getSegmentCount: dom = " ++ show dom ++ " domWidthScreen = " ++ show domWidthScreen) $
                    let ?mixedMultInOutEffort = effMultInt in
                    case ArithUpDn.convertUpEff effToInt 0 (segPerUnit |<*> domWidthScreen) of
                        Just cnt -> (cnt :: Int)
                    where
                    domWidthScreen = 
                        let ?addInOutEffort = effAdd in
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
        let ?joinmeetEffort = effJoinMeet in
        dom <\/> (vdomLO </\> vdomHI)
        where
        (_,_,vdomLO, vdomHI) = getVisibleDomExtents coordSystem
        (<\/>) = RefOrd.joinOutEff effJoinMeet
        (</\>) = RefOrd.meetOutEff effJoinMeet
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
    
    sampleF = fn
    sampleDF = dom
    sampleI = 1 :: Int
    
    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDF effReal
    effJoinMeet = ArithInOut.rrEffortJoinMeet sampleDF effReal
    effToInt = ArithInOut.rrEffortToInt sampleDF effReal
    effAdd =
        ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effReal
    effMultInt =
        ArithInOut.mxfldEffortMult sampleDF sampleI $ ArithInOut.rrEffortIntMixedField sampleDF effReal
    effDivInt =
        ArithInOut.mxfldEffortDiv sampleDF sampleI $ ArithInOut.rrEffortIntMixedField sampleDF effReal
    
    