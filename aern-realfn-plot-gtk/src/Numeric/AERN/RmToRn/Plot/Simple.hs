{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.Simple
    Description :  simple utilities for plotting functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Simple utilities for plotting functions.
-}
module Numeric.AERN.RmToRn.Plot.Simple
(
    plotFns,
    simpleFnMetaData
)
where

import Numeric.AERN.RmToRn.Plot.FnView.FnData
import Numeric.AERN.RmToRn.Plot.FnView.New
import Numeric.AERN.RmToRn.Plot.Params

import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent.STM

{-|
    To make the following plotting code work, the file FnView.glade
    has to be available in the current directory.  The master copy of this file
    is in the root folder of the aern-realfn-plot-gtk package.
-}
plotFns ::
    (RoundedRealFn f, 
     CairoDrawableFn f,
     Show f, Show (Domain f), Show (Var f)) 
    =>
    [(String, [(String, f)])] -> IO ()
plotFns fnGroups =
    do
    -- enable multithreaded GUI:
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    where
    ((sampleFn :_) :_) = fns 
    sampleCF = getSampleDomValue sampleFn
    _varDoms@((var, dom) : _) = getVarDoms sampleFn
    (domL, domR) = RefOrd.getEndpointsOut dom
    effDrawFn = cairoDrawFnDefaultEffort sampleFn
    effEval = evaluationDefaultEffort sampleFn
    effCF = ArithInOut.roundedRealDefaultEffort sampleCF
    --effCF = (100, (100,())) -- MPFR

    (groupNames, groupFns) = unzip fnGroups
    (fnNames, fns) = unzip $ map unzip groupFns 
    fnmeta =
        (defaultFnMetaData sampleFn)
        {
            dataFnGroupNames = groupNames,
            dataFnNames = fnNames,
            dataFnStyles = map mkStyles fns,
            dataDomName = show var,
            dataDomL = domL,
            dataDomR = domR
        }
        where
        mkStyles fs = map snd $ zip fs colours
        colours = cycle [black, red, green, blue]
        black = defaultFnPlotStyle
        red = defaultFnPlotStyle 
            { 
                styleOutlineColour = Just (0.8,0.2,0.2,1), 
                styleFillColour = Just (0.8,0.2,0.2,0.1) 
            } 
        green = defaultFnPlotStyle 
            { 
                styleOutlineColour = Just (0.2,0.8,0.2,1), 
                styleFillColour = Just (0.2,0.8,0.2,0.1) 
            } 
        blue = defaultFnPlotStyle 
            { 
                styleOutlineColour = Just (0.1,0.1,0.8,1), 
                styleFillColour = Just (0.1,0.1,0.8,0.1) 
            } 

    
simpleFnMetaData :: 
      (fnInfo ~ (String, FnPlotStyle, Bool), 
       HasZero (Domain t), HasOne (Domain t), HasDomainBox t) 
      =>
      t
      -> Rectangle (Domain f) -- ^ initial crop
      -> Maybe ColourRGBA -- ^ background colour
      -> Int -- ^ number of samples to take of each function per viewable region
      -> [(String, [fnInfo])] -- ^ information on plotted function groups (names, plot colours, whether shown initially)
      -> FnMetaData f
simpleFnMetaData sampleFn rect bgrColour samplesPerUnit (groups :: [(String, [fnInfo])]) =
        (defaultFnMetaData sampleFn)
        {
            dataFnGroupNames = map getGroupName groups,
            dataFnNames = mapGroupsFns getFnName, 
            dataFnStyles = mapGroupsFns getFnStyle,
            dataDefaultActiveFns = mapGroupsFns getFnEnabled,
            dataDomL = domL,
            dataDomR = domR,
            dataValLO = valLO,
            dataValHI = valHI,
            dataDefaultEvalPoint = domR,
            dataDefaultCanvasParams =
                (defaultCanvasParams sampleDom)
                {
                    cnvprmCoordSystem = CoordSystemLinear rect,
                    cnvprmBackgroundColour = bgrColour,
                    cnvprmSamplesPerUnit = samplesPerUnit
                }
        }
        where
        (Rectangle valHI valLO domL domR) = rect
        sampleDom = getSampleDomValue sampleFn
        getGroupName (name, _) = name
        getGroupContent (_, content) = content
        mapGroupsFns :: (fnInfo -> t) -> [[t]]
        mapGroupsFns f = map (map f . getGroupContent) groups 
        getFnName (name, _, _) = name
        getFnStyle (_, style, _) = style
        getFnEnabled (_, _, enabled) = enabled
    

addPlotVar :: 
    (HasDomainBox f)
    =>
    [[f]] -> [[(GraphOrParamPlotFn f, Var f)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (GraphPlotFn fn, plotVar)
        where
        (plotVar : _) = vars
        vars = map fst $ getVarDoms fn   
    