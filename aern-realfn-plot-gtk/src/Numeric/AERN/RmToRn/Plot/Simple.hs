{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.Simple
    Description :  simple utilities for plotting functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Abstraction for drawing on a Cairo canvas.
-}
module Numeric.AERN.RmToRn.Plot.Simple
(
    plotFns
)
where

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

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
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    where
    ((sampleFn :_) :_) = fns 
    sampleCF = getSampleDomValue sampleFn
    effDrawFn = cairoDrawFnDefaultEffort sampleFn
    effEval = evaluationDefaultEffort sampleFn
    effCF = ArithInOut.roundedRealDefaultEffort sampleCF
    --effCF = (100, (100,())) -- MPFR

    (groupNames, groupFns) = unzip fnGroups
    (fnNames, fns) = unzip $ map unzip groupFns 
    fnmeta =
        (FV.defaultFnMetaData sampleFn)
        {
            FV.dataFnGroupNames = groupNames,
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map mkStyles fns
        }
        where
        mkStyles fs = map snd $ zip fs colours
        colours = cycle [black, red, green, blue]
        black = FV.defaultFnPlotStyle
        red = FV.defaultFnPlotStyle 
            { 
                FV.styleOutlineColour = Just (0.8,0.2,0.2,1), 
                FV.styleFillColour = Just (0.8,0.2,0.2,0.1) 
            } 
        green = FV.defaultFnPlotStyle 
            { 
                FV.styleOutlineColour = Just (0.2,0.8,0.2,1), 
                FV.styleFillColour = Just (0.2,0.8,0.2,0.1) 
            } 
        blue = FV.defaultFnPlotStyle 
            { 
                FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
                FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
            } 

    
    
addPlotVar :: 
    (HasDomainBox f)
    =>
    [[f]] -> [[(f, Var f)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (fn, plotVar)
        where
        (plotVar : _) = vars
        vars = map fst $ getVarDoms fn   
    