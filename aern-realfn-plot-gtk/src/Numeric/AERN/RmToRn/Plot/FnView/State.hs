{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.FnView.State
    Description :  internal state of a FnView widget
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for FnView.
    Internal state of a FnView widget.
-}
module Numeric.AERN.RmToRn.Plot.FnView.State
--(
--)
where

import Numeric.AERN.RmToRn.Plot.FnView.FnData

import Numeric.AERN.RmToRn.Plot.Params

import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug

data FnViewState f =
    FnViewState
    {
        favstActiveFns :: [[Bool]],
        favstTrackingDefaultEvalPt :: Bool,
        favstCanvasParams :: CanvasParams (Domain f),
        favstZoomPercent :: Double,
        favstPanCentre :: (Domain f, Domain f) 
    }
    
initState :: 
    (ArithInOut.RoundedReal (Domain f))
    =>
    (ArithInOut.RoundedRealEffortIndicator (Domain f)) ->
    (t, FnMetaData f) -> 
    FnViewState f
initState effReal (_, fnmeta) =
    FnViewState
    {
        favstActiveFns = activeFns,
        favstTrackingDefaultEvalPt = True,
        favstCanvasParams = dataDefaultCanvasParams fnmeta,
        favstZoomPercent = defaultZoom,
        favstPanCentre = getDefaultCentre effReal fnmeta
    }
    where
    activeFns = mergeDefaults (mergeDefaults $ \ a b -> b) allTrue $ dataDefaultActiveFns fnmeta
    allTrue = map (map $ const True) $ dataFnNames fnmeta
    mergeDefaults :: (a -> a -> a) -> [a] -> [a] -> [a]
    mergeDefaults _ [] _ = [] 
    mergeDefaults _ list1 [] = list1
    mergeDefaults mergeElems (h1:t1) (h2:t2) =
        (mergeElems h1 h2) : (mergeDefaults mergeElems t1 t2)
    
defaultZoom :: Double
defaultZoom = 95
    
updateZoomPanCentreCoordSystem ::
    Double ->
    (Domain f, Domain f) ->
    (CoordSystem (Domain f)) ->
    (FnViewState f) ->
    (FnViewState f)
updateZoomPanCentreCoordSystem zoomPercent panCentre coordSystem state = 
    state 
    { 
        favstCanvasParams = 
            (favstCanvasParams state) 
                { cnvprmCoordSystem = coordSystem },
        favstZoomPercent = zoomPercent,
        favstPanCentre = panCentre
    }
    
updatePanCentreCoordSystem ::
    (Domain f, Domain f) ->
    (CoordSystem (Domain f)) ->
    (FnViewState f) ->
    (FnViewState f)
updatePanCentreCoordSystem = updateZoomPanCentreCoordSystem defaultZoom

updateZoomPercentAndFnExtents effFromDouble zoomPercent fnExtents state =
    state
    {
        favstCanvasParams = 
            (favstCanvasParams state) 
                { cnvprmCoordSystem = newCoordSystem },
        favstZoomPercent = zoomPercent
    }
    where
    newCoordSystem =
        case cnvprmCoordSystem (favstCanvasParams state) of
            csys@(CoordSystemLogSqueeze _) -> 
                csys
            CoordSystemLinear _ ->
                linearCoordsWithZoomAndCentre effFromDouble zoomPercent centre fnExtents
    centre = favstPanCentre state

updateCentreByRatio ::
    (ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f))
    =>
    (ArithInOut.RoundedRealEffortIndicator (Domain f)) -> 
    (Double, Double) ->
    FnViewState f -> 
    FnViewState f
updateCentreByRatio effReal (ratX, ratY) state =
    case cnvprmCoordSystem (favstCanvasParams state) of
        CoordSystemLogSqueeze _ -> state
        CoordSystemLinear (Rectangle hi lo l r) ->
--            unsafePrint (
--                "updateCentreByRatio: CoordSystemLinear: "
--                ++ "\n ratX = " ++ show ratX
--                ++ "; ratY = " ++ show ratY
--                ++ "\n shiftX = " ++ show shiftX
--                ++ "; shiftY = " ++ show shiftY
--                ++ "\n old cX = " ++ show cX
--                ++ "; old cY = " ++ show cY
--                ++ "\n old Rect = " ++ show (hi,lo,l,r)
--                ++ "\n new system = " ++ show coordSystem
--            ) $ 
            state
            {
                favstCanvasParams = 
                    (favstCanvasParams state) 
                        { cnvprmCoordSystem = coordSystem },
                favstPanCentre = (shiftX cX, shiftY cY)
            }
            where
            (cX,cY) = favstPanCentre state
            shiftX a =
                let ?addInOutEffort = effAdd in
                let ?mixedMultInOutEffort = effMultDbl in 
                fst $ RefOrd.getEndpointsOut $ 
                    a <-> (ratX |<*> fnDomWidth)
            shiftY a = 
                let ?addInOutEffort = effAdd in
                let ?mixedMultInOutEffort = effMultDbl in
                fst $ RefOrd.getEndpointsOut $ 
                    a <-> (ratY |<*> fnRangeHeight)
            fnDomWidth = 
                let ?addInOutEffort = effAdd in
                r <-> l
            fnRangeHeight = 
                let ?addInOutEffort = effAdd in
                lo <-> hi
            coordSystem =
                CoordSystemLinear 
                    (Rectangle 
                        (shiftY hi) (shiftY lo) 
                        (shiftX l) (shiftX r))
            sampleDom = cX
            effAdd =
                ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effReal
            effMultDbl =
                ArithInOut.mxfldEffortMult sampleDom (1::Double) $ ArithInOut.rrEffortDoubleMixedField sampleDom effReal
            
    
updateFnActive :: 
    Int -> 
    Int -> 
    Bool -> 
    (FnViewState f) -> 
    (FnViewState f)
updateFnActive fnNo dimNo isActive state =
    state
    {
        favstActiveFns = updateDim $ favstActiveFns state
    }
    where
    updateDim activeDims =
        listUpdate fnNo activeFnDims activeDims
        where
        activeFnDims =
            listUpdate dimNo isActive (activeDims !! fnNo)

linearCoordsWithZoom ::
    (ArithInOut.RoundedReal t)
    =>
    ArithInOut.RoundedRealEffortIndicator t ->
    Double {-^ zoom level in percent -}  ->
    (t,t,t,t) 
        {-^ upper, lower, left, right bounds of the function graph -} ->
    CoordSystem t
linearCoordsWithZoom effReal zoomPercent fnExtents@(fnHI, fnLO, fnL, fnR) =
    linearCoordsWithZoomAndCentre effReal zoomPercent (cX,cY) fnExtents
    where
    cX = 
        let ?addInOutEffort = effAdd in
        let ?mixedDivInOutEffort = effDivInt in
        (fnL <+> fnR) </>| (2 :: Int) 
    cY = 
        let ?addInOutEffort = effAdd in
        let ?mixedDivInOutEffort = effDivInt in
        (fnLO <+> fnHI) </>| (2 :: Int) 
    sampleDom = fnL
    effAdd =
        ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effReal
    effDivInt =
        ArithInOut.mxfldEffortDiv sampleDom (1::Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effReal

linearCoordsWithZoomAndCentre ::
    (ArithInOut.RoundedReal t)
    =>
    ArithInOut.RoundedRealEffortIndicator t ->
    Double {-^ zoom level in percent -}  ->
    (t, t) {-^ x,y coordinates of the centre -}  ->
    (t, t, t, t) 
        {-^ upper, lower, left, right bounds of the function graph -} ->
    CoordSystem (t)
linearCoordsWithZoomAndCentre effReal zoomPercent (cX,cY) (fnHI, fnLO, fnL, fnR) =
    CoordSystemLinear $ Rectangle hi lo l r
    where
    hi = 
        let ?addInOutEffort = effAdd in
        cY <+> heighHalf
    lo = 
        let ?addInOutEffort = effAdd in
        cY <-> heighHalf
    l = 
        let ?addInOutEffort = effAdd in
        cX <-> widthHalf
    r = 
        let ?addInOutEffort = effAdd in
        cX <+> widthHalf
    heighHalf = 
        let ?mixedMultInOutEffort = effMultDbl in
        zoomRatio |<*> fnHeightHalf
    widthHalf = 
        let ?mixedMultInOutEffort = effMultDbl in
        zoomRatio |<*> fnWidthHalf
    zoomRatio = 100 / zoomPercent
    fnWidthHalf = 
        let ?addInOutEffort = effAdd in
        let ?mixedDivInOutEffort = effDivInt in
        (fnR <-> fnL) </>| (2 :: Int)
    fnHeightHalf = 
        let ?addInOutEffort = effAdd in
        let ?mixedDivInOutEffort = effDivInt in
        (fnHI <-> fnLO) </>| (2 :: Int)

    sampleDom = cX
    effAdd =
        ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effReal
    effMultDbl =
        ArithInOut.mxfldEffortMult sampleDom (1::Double) $ 
            ArithInOut.rrEffortDoubleMixedField sampleDom effReal
    effDivInt =
        ArithInOut.mxfldEffortDiv sampleDom (1::Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effReal

    
listUpdate :: Int -> a -> [a] -> [a]
listUpdate i newx (x:xs) 
    | i == 0 = newx : xs
    | i > 0 = x : (listUpdate (i - 1) newx xs) 

