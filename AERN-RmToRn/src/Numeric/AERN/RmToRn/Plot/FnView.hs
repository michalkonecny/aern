{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef GLADE_DIR
#else
#define GLADE_DIR "./"
#endif 
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.FnView
    Description :  a gtk widget inspecting cairo drawable functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    A gtk widget inspecting function enclosures.
-}
module Numeric.AERN.RmToRn.Plot.FnView 
(
    FnData (..),
    FnMetaData (..),
    defaultFnData,
    defaultFnMetaData,
    new,
    module Numeric.AERN.RmToRn.Plot.Params
)
where

import Numeric.AERN.RmToRn.Plot.FnView.FnData
import Numeric.AERN.RmToRn.Plot.FnView.State
import Numeric.AERN.RmToRn.Plot.FnView.Layout
import Numeric.AERN.RmToRn.Plot.FnView.UpdateView

import Numeric.AERN.RmToRn.Plot.Params
import Numeric.AERN.RmToRn.Plot.CairoDrawable (CairoDrawableFn(..))

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.Gdk.EventM as GdkEv

import Control.Concurrent (forkIO)
import Control.Concurrent.STM -- as STM
import Data.IORef

import qualified System.FilePath as FilePath

{-|
    Create a new viewer linked to the given data variable.
-}
new ::
    (CairoDrawableFn f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     HasEvalOps f (Domain f),
     RefOrd.PartialComparison (Domain f),
     Show f, Show (Domain f))
    =>
    f {-^ sample value -} ->
    (CairoDrawFnEffortIndicator f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    EvalOpsEffortIndicator f (Domain f) ->
    (TVar (FnData f),
     TVar (FnMetaData f)) ->
    (Maybe Gtk.Window) {- ^ parent window -} -> 
    IO Gtk.Window
new sampleF effDraw effReal effEval fndataTVs@(fndataTV, fnmetaTV) maybeParentWindow =
    do
    -- create initial state objects:
    stateTV <- atomically $
        do
        fndata <- readTVar fndataTV
        fnmeta <- readTVar fnmetaTV
        newTVar $ initState effReal (fndata, fnmeta)
    dynWidgetsRef <- newIORef initFnViewDynWidgets
    -- create most widgets:
    widgets <- loadGlade (FilePath.combine GLADE_DIR "FnView.glade")
    -- create plotting canvas:
    widgets <- makeCanvas sampleF effDraw effToDouble widgets fndataTVs stateTV
    -- attach handlers to widgets
    Gtk.onDestroy (window widgets) $
        do
        atomically $ modifyTVar fnmetaTV $ \fnmeta -> fnmeta { dataDestroyed = True }
        Gtk.mainQuit
    setHandlers sampleF effReal effEval widgets dynWidgetsRef fndataTVs stateTV
    -- start thread that reponds to changes in fndataTVs:
    forkIO $
        dataWatchThread 
            sampleF effReal effEval 
            widgets dynWidgetsRef fndataTVs stateTV
    Gtk.widgetShowAll $ window widgets
    return $ window widgets
    where
    sampleDom = getSampleDomValue sampleF
    effToDouble = ArithInOut.rrEffortToDouble sampleDom effReal

makeCanvas sampleF effDraw effToDouble widgets fndataTVs@(fadataTV, fndataTV) stateTV =
    do
    -- create canvas:
    canvas <- Gtk.drawingAreaNew
    -- set canvas properties:
--    Gtk.onRealize canvas $
--        do
----        Gtk.widgetModifyBg canvas Gtk.StateNormal 
----        Gtk.set canvas []
--        return () 
    -- open font for labels:
    let font = () -- a dummy
    -- set the canvas repaint handler:
    Gtk.onExpose canvas $ \ event ->
        do
        (fndatas, state) <- atomically $
            do
            fadata <- readTVar fadataTV
            fndata <- readTVar fndataTV
            state <- readTVar stateTV
            return ((fadata, fndata), state)  
        repaintCanvas sampleF effDraw effToDouble canvas font fndatas state
--    Gtk.timeoutAdd (Gtk.widgetQueueDraw canvas >> return True) 500 >> return ()
    -- plug the canvas in the GUI:
    Gtk.set (canvasAlignment widgets)
        [ Gtk.containerChild := canvas ]
    return $ widgets { canvas = canvas }
    
repaintCanvas ::
    (CairoDrawableFn f,
     HasDomainBox f,
     ArithUpDn.Convertible (Domain f) Double,
     Show (Domain f))
    =>
    f ->
    (CairoDrawFnEffortIndicator f) ->
    (ArithUpDn.ConvertEffortIndicator (Domain f) Double) ->
    Gtk.DrawingArea ->
--    FTGL.Font ->
    () ->
    ((FnData f),
     FnMetaData f) ->
    (FnViewState f) ->
    IO Bool
repaintCanvas (sampleF :: f) effDraw effToDouble da _font (fndata, fnmeta) state =
    do
    putStrLn "repaintCanvas: starting"
    drawWin <- Gtk.widgetGetDrawWindow da
    (wi,hi) <- Gtk.widgetGetSize da
    let w = realToFrac wi; h = realToFrac hi
    Gtk.renderWithDrawable drawWin $
        do
--        background w h
        -- draw outlines of all function enclosures: 
        mapM_ (drawFn w h) $ collectFns fnsActive fns fnsColours 
--    putStrLn "repaintCanvas: done"
    return True
    where
--    background w h = 
--        do
--        moveTo 0 0
--        mapM_ (uncurry lineTo) [(w,0),(w,h),(0,h),(0,0)]
--        setSourceRGBA 0.8 0.8 0.8 1 -- background colour
--        fill
    plotParams = favstPlotParams state
    fnsActive = concat $ favstActiveFns state
    fnsColours = concat $ dataFnColours fnmeta
    fns = concat $ dataFns fndata
    
    collectFns [] _ _ = []
    collectFns (False:restActive) (_:restFns) (_:restColours) = 
        collectFns restActive restFns restColours
    collectFns (True:restActive) (fn:restFns) (col:restColours) = 
        (fn, col) : (collectFns restActive restFns restColours)
    
    drawFn ::
        Double -> Double ->
        (f, ColourRGBA) ->
        Render ()
    drawFn wi hi (fn, color@(r,g,b,a)) =
        cairoDrawFn effDraw plotParams (toScreenCoords wi hi) (Just color) (Just (r,g,b,0.1)) fn
        where
        _ = [sampleF, fn]
    toScreenCoords ::
        Double -> Double ->
        (Domain f, Domain f) ->
        (Double, Double)        
    toScreenCoords w h (xUnit,yUnit) =
        (w*xUnitD,h*(1-yUnitD)) -- Cairo's origin is the top left corner 
        where
        _ = [sampleDom, xUnit, yUnit]
        Just xUnitD = ArithUpDn.convertUpEff effToDouble xUnit
        Just yUnitD = ArithUpDn.convertUpEff effToDouble yUnit
    sampleDom = getSampleDomValue sampleF


setHandlers :: 
    (ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     HasEvalOps f (Domain f), 
     Show f, Show (Domain f))
    =>
    f {-^ sample value -} ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    EvalOpsEffortIndicator f (Domain f) ->
    Widgets -> 
    IORef FnViewDynWidgets -> 
    (TVar (FnData f), TVar (FnMetaData f)) -> 
    TVar (FnViewState f) -> 
    IO ()
setHandlers (sampleF :: f) effReal effEval widgets dynWidgetsRef fndataTVs@(fndataTV, fnmetaTV) stateTV =
    do
    setHandlerPrintTXTButton
    setHandlerDefaultEvalPointButton
    setHandlerEvalPointEntry
    setHandlerCoordSystem
    setHandlerZoomAndPanEntries
    setHandlerPanByMouse
    setHandlerZoomByMouse
    state <- atomically $ readTVar stateTV
    updateZoomWidgets toDbl widgets state
--    putStrLn $ "setHandlers: " ++ (show $ pltprmCoordSystem $ favstPlotParams state)
    return ()
    where        
    toDbl :: (Domain f) -> Double
    toDbl a = d
        where
        (Just d) = ArithUpDn.convertUpEff effToDouble a
    effToDouble = ArithInOut.rrEffortToDouble sampleDom effReal
    effFromDouble = ArithInOut.rrEffortFromDouble sampleDom effReal
    sampleDom = getSampleDomValue sampleF
    
    setHandlerCoordSystem =
        do
        Gtk.on (coorSystemCombo widgets) Gtk.changed resetZoomPanFromCoordSystem
        Gtk.onClicked (defaultZoomPanButton widgets) resetZoomPanFromCoordSystem
        where
        resetZoomPanFromCoordSystem =
            do
            maybeCSysIx <- Gtk.comboBoxGetActive (coorSystemCombo widgets)
            putStrLn $ "resetZoomPanFromCoordSystem: maybeCSysIx = " ++ show maybeCSysIx
            case maybeCSysIx of
                -1 -> return ()
                ix ->
                    do
                    state <-
                        atomically $
                            do
                            fnmeta <- readTVar fnmetaTV
                            let coordSystem = case ix of
                                    0 -> CoordSystemLogSqueeze sampleDom
                                    1 -> 
                                        linearCoordsWithZoom effReal defaultZoom (getFnExtents fnmeta)
                            state <- modifyTVar stateTV $ 
                                updatePanCentreCoordSystem (getDefaultCentre effReal fnmeta) coordSystem
                            return state
                    Gtk.widgetQueueDraw (canvas widgets)
                    updateZoomWidgets toDbl widgets state
        
    setHandlerZoomAndPanEntries =
        do
        Gtk.onEntryActivate (zoomEntry widgets) (zoomHandler ())
        Gtk.onFocusOut (zoomEntry widgets) (\ e -> zoomHandler False)
        Gtk.onEntryActivate (centreXEntry widgets) (zoomHandler ())
        Gtk.onFocusOut (centreXEntry widgets) (\ e -> zoomHandler False)
        Gtk.onEntryActivate (centreYEntry widgets) (zoomHandler ())
        Gtk.onFocusOut (centreYEntry widgets) (\ e -> zoomHandler False)
        where
        zoomHandler returnValue =
            do
            zoomS <- Gtk.entryGetText (zoomEntry widgets)
            centreXS <- Gtk.entryGetText (centreXEntry widgets)
            centreYS <- Gtk.entryGetText (centreYEntry widgets)
            case (reads zoomS, reads centreXS, reads centreYS)  of
                ([(zoomPercent,"")], [(centreXD,"")], [(centreYD,"")]) -> 
                    atomically $
                        do
                        fnmeta <- readTVar fnmetaTV
                        modifyTVar stateTV $ 
                            updateZoomPanCentreCoordSystem zoomPercent (centreX, centreY) $ 
                                linearCoordsWithZoomAndCentre effReal zoomPercent (centreX, centreY) $
                                    getFnExtents fnmeta 
                        return ()
                    where
                    centreX = ArithInOut.convertOutEff effFromDouble (centreXD :: Double)
                    centreY = ArithInOut.convertOutEff effFromDouble (centreYD :: Double)
                wrongParseResults ->
                    do
                    putStrLn $ "zoomHandler: parse error: " ++ show wrongParseResults 
                    return ()
--            putStrLn $ "zoomHandler"
            Gtk.widgetQueueDraw (canvas widgets)
            return returnValue

    setHandlerPanByMouse =
        do
        -- a variable to keep track of position before each drag movement:
        panOriginTV <- atomically $ newTVar Nothing
        -- setup the canvas to receive various mouse events:
        Gtk.widgetAddEvents (canvas widgets)
            [Gtk.ButtonPressMask, Gtk.ButtonReleaseMask, Gtk.PointerMotionMask]
        -- what to do when the left mouse button is pressed:
        Gtk.on (canvas widgets) Gtk.buttonPressEvent $
            do
            button <- GdkEv.eventButton
            coords <- GdkEv.eventCoordinates
            case button of
                GdkEv.LeftButton ->
                    liftIO $
                        do
                        -- remember the position and indicate that dragging is underway:
                        atomically $ writeTVar panOriginTV (Just coords)
                        return ()
                _ -> return ()
            return False
        -- what to do when the left mouse button is released:
        Gtk.on (canvas widgets) Gtk.buttonReleaseEvent $
            do
            button <- GdkEv.eventButton 
            case button of
                GdkEv.LeftButton ->
                    liftIO $
                        do
                        -- indicate no dragging is underway:
                        atomically $ writeTVar panOriginTV Nothing
                        return ()
                _ -> return ()
            return False
        -- what to do when mouse moves:
        Gtk.on (canvas widgets) Gtk.motionNotifyEvent $
            do
            coords@(x,y) <- GdkEv.eventCoordinates
            liftIO $
                do
                -- update the dragging information variable:
                maybePanOrigin <- atomically $
                    do
                    maybePanOrigin <- readTVar panOriginTV
                    case maybePanOrigin of
                        Nothing ->
                            return maybePanOrigin
                        Just _ ->
                            do
                            writeTVar panOriginTV (Just coords)
                            return maybePanOrigin
                -- check whether dragging or not:
                case maybePanOrigin of
                    Nothing -> return ()
                    Just panOrigin@(oX,oY) -> -- yes, dragging occurred
                        do
                        -- find out the size of the canvas:
                        (canvasX, canvasY) <- Gtk.widgetGetSize (canvas widgets)
                        -- recalculate the centre and coordinate bounds 
                        -- based on the drag amount relative to the size fo the canvas:
                        state <- atomically $ modifyTVar stateTV $
                            updateCentreByRatio
                                effReal
                                ((x - oX) / (int2dbl canvasX), 
                                 (y - oY) / (int2dbl canvasY))
--                        putStrLn $ "motionNotifyEvent: coords = " ++ show coords 
--                            ++ "; panOrigin = " ++ show panOrigin
--                            ++ "; (canvasX,canvasY) = " ++ show (canvasX,canvasY)
                        return ()
                        -- make sure the text in the zoom and pan entries are updated:
                        updateZoomWidgets toDbl widgets state
                        -- schedule the canvas for redraw:
                        Gtk.widgetQueueDraw (canvas widgets)
                        where
                        int2dbl :: Int -> Double
                        int2dbl = realToFrac
            return False
        return ()
            
    setHandlerZoomByMouse =
        do -- IO
        Gtk.widgetAddEvents (canvas widgets) [Gtk.ScrollMask]
        Gtk.on (canvas widgets) Gtk.scrollEvent $
            do -- ReaderTV
            scrollDirection <- GdkEv.eventScrollDirection
            liftIO $
                do -- IO
                state <- atomically $
                    do -- STM
                    state <- readTVar stateTV 
                    let zoomPercent = favstZoomPercent state
                    let newZoomPercent = case scrollDirection of
                                            GdkEv.ScrollUp ->  1.25 * zoomPercent
                                            GdkEv.ScrollDown -> 0.8 * zoomPercent
                                            _ -> zoomPercent
                    fnmeta <- readTVar fnmetaTV
                    state <- modifyTVar stateTV $ 
                        updateZoomPercentAndFnExtents effReal newZoomPercent $ getFnExtents fnmeta
                    return state
                updateZoomWidgets toDbl widgets state
                Gtk.widgetQueueDraw (canvas widgets)
                return False
        return () -- TODO
            
    setHandlerPrintTXTButton =
        Gtk.onClicked (printTXTButton widgets) $
            do
            (state, FnData fas) <- 
                atomically $
                    do
                    state <- readTVar stateTV
                    fas <- readTVar fndataTV
                    return (state, fas)
            putStrLn $
                unlines $ map show $ fas
    setHandlerDefaultEvalPointButton =
        Gtk.onClicked (defaultEvalPointButton widgets) $
            do
            (state, fnmeta) <- 
                atomically $
                    do
                    state <- readTVar stateTV
                    fnmeta <- readTVar fnmetaTV
                    return (state, fnmeta)
            case favstTrackingDefaultEvalPt state of
                False ->
                    do
                    Gtk.entrySetText (evalPointEntry widgets) $ 
                        show $ dataDefaultEvalPoint fnmeta
                    atomically $ modifyTVar stateTV $
                        \ st -> st { favstTrackingDefaultEvalPt = True }
                    updateValueDisplayTV effFromDouble effEval widgets dynWidgetsRef fndataTVs stateTV 
                True -> -- already tracking the default
                    return ()
    setHandlerEvalPointEntry =
        do
        Gtk.onEntryActivate (evalPointEntry widgets) $ 
            do
            updateEvalPointHandler
        Gtk.onFocusOut (evalPointEntry widgets) $ \ _ -> 
            do
            updateEvalPointHandler
            return False
        where
        updateEvalPointHandler =
            do
            -- indicate that we no longer wish to track the default point:  
            atomically $ modifyTVar stateTV $ 
                \ st -> st { favstTrackingDefaultEvalPt = False }
            -- update the values for the new point:  
            updateValueDisplayTV effFromDouble effEval widgets dynWidgetsRef fndataTVs stateTV

    

    