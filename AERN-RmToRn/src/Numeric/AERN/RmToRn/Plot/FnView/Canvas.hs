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
    Module      :  Numeric.AERN.RmToRn.Plot.Canvas
    Description :  the widget where functions are plotted
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for FnView.
    The widget where functions are plotted.
-}
module Numeric.AERN.RmToRn.Plot.FnView.Canvas
where

import Numeric.AERN.RmToRn.Plot.FnView.FnData
import Numeric.AERN.RmToRn.Plot.FnView.State
import Numeric.AERN.RmToRn.Plot.FnView.Layout

import Numeric.AERN.RmToRn.Plot.Params
import Numeric.AERN.RmToRn.Plot.CairoDrawable (CairoDrawableFn(..))

import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

--import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.Gdk.EventM as GdkEv

import Control.Concurrent.STM -- as STM

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
--    putStrLn "repaintCanvas: starting"
    drawWin <- Gtk.widgetGetDrawWindow da
    (wi,hi) <- Gtk.widgetGetSize da
    let w = realToFrac wi; h = realToFrac hi
    Gtk.renderWithDrawable drawWin $
        do
--        background w h
        -- draw outlines of all function enclosures:
        drawFunctions sampleF effDraw effToDouble plotParams w h fnsActive fns fnsStyles
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
    fnsStyles = concat $ dataFnStyles fnmeta
    fns = concat $ dataFns fndata
    
drawFunctions ::
    (CairoDrawableFn f,
     HasDomainBox f,
     ArithUpDn.Convertible (Domain f) Double,
     Show (Domain f))
    =>
    f ->
    (CairoDrawFnEffortIndicator f) ->
    (ArithUpDn.ConvertEffortIndicator (Domain f) Double) ->
    PlotParams (Domain f) ->
    Double -> Double ->
    [Bool] ->
    [f] ->
    [FnPlotStyle] ->
    Render ()
drawFunctions (sampleF :: f) effDraw effToDouble plotParams w h fnsActive fns fnsStyles =
    do
    mapM_ drawFn $ collectFns fnsActive fns fnsStyles
    drawAxisLabels
    where
    collectFns [] _ _ = []
    collectFns (False:restActive) (_:restFns) (_:restStyles) = 
        collectFns restActive restFns restStyles
    collectFns (True:restActive) (fn:restFns) (col:restStyles) = 
        (fn, col) : (collectFns restActive restFns restStyles)
    
    drawFn ::
        (f, FnPlotStyle) ->
        Render ()
    drawFn (fn, style) =
        cairoDrawFn effDraw plotParams toScreenCoords style fn
        
    drawAxisLabels =
        do
        return ()
    
    toScreenCoords ::
        (Domain f, Domain f) ->
        (Double, Double)        
    toScreenCoords (xUnit,yUnit) =
        (w*xUnitD,h*(1-yUnitD)) -- Cairo's origin is the top left corner 
        where
        _ = [sampleDom, xUnit, yUnit]
        Just xUnitD = ArithUpDn.convertUpEff effToDouble xUnit
        Just yUnitD = ArithUpDn.convertUpEff effToDouble yUnit
    sampleDom = getSampleDomValue sampleF


