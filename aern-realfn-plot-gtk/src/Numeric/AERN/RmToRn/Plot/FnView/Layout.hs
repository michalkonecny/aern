{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.FnView.Layout
    Description :  layout of the FnView widget
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for FnView.
    Layout of the FnView widget. 
-}
module Numeric.AERN.RmToRn.Plot.FnView.Layout
--(
--)
where

import Numeric.AERN.RmToRn.Plot.FnView.State

import Numeric.AERN.RmToRn.Plot.Params

import qualified Graphics.UI.Gtk as Gtk

import System.Directory (doesFileExist)

loadGlade :: 
    FilePath ->
    IO Widgets
loadGlade gladeFileName =
    do
    gotGladeFile <- doesFileExist gladeFileName
    case gotGladeFile of
        True -> return ()
        False -> error $ "AERN: RmToRn.Plot.FnView: glade file " ++ gladeFileName ++ " not found" 
    gui <- Gtk.builderNew
    Gtk.builderAddFromFile gui gladeFileName
    window <- Gtk.builderGetObject gui Gtk.castToWindow "window1"
    canvasAlignment <- Gtk.builderGetObject gui Gtk.castToAlignment "canvasAlignment1"
    coorSystemCombo <- Gtk.builderGetObject gui Gtk.castToComboBox "coorSystemCombo1"
    evalPointEntry <- Gtk.builderGetObject gui Gtk.castToEntry "evalPointEntry1"
    defaultEvalPointButton <- Gtk.builderGetObject gui Gtk.castToButton "defaultEvalPointButton1"
    dimTable <- Gtk.builderGetObject gui Gtk.castToTable "dimTable1"
    domVarLabel <- Gtk.builderGetObject gui Gtk.castToLabel "domVarLabel1"
    zoomEntry <- Gtk.builderGetObject gui Gtk.castToEntry "zoomEntry1"
    defaultZoomPanButton <- Gtk.builderGetObject gui Gtk.castToButton "defaultZoomPanButton1"
    centreXEntry <- Gtk.builderGetObject gui Gtk.castToEntry "centreXEntry1"
    centreYEntry <- Gtk.builderGetObject gui Gtk.castToEntry "centreYEntry1"
    exportPNGButton <- Gtk.builderGetObject gui Gtk.castToButton "exportPNGButton1"
    exportSVGButton <- Gtk.builderGetObject gui Gtk.castToButton "exportSVGButton1"
    exportPDFButton <- Gtk.builderGetObject gui Gtk.castToButton "exportPDFButton1"
    printTXTButton <- Gtk.builderGetObject gui Gtk.castToButton "printTXTButton1"
    return $ Widgets
        {
            window = window,
            canvasAlignment = canvasAlignment,
            coorSystemCombo = coorSystemCombo,
            evalPointEntry = evalPointEntry,
            defaultEvalPointButton = defaultEvalPointButton,
            dimTable = dimTable,
            domVarLabel = domVarLabel,
            zoomEntry = zoomEntry,
            defaultZoomPanButton = defaultZoomPanButton,
            centreXEntry = centreXEntry,
            centreYEntry = centreYEntry,
            exportPNGButton = exportPNGButton,
            exportSVGButton = exportSVGButton,
            exportPDFButton = exportPDFButton,
            printTXTButton = printTXTButton,
            canvas = error "canvas not created yet"
        }

data Widgets = 
    Widgets
    {
        window :: Gtk.Window,
        canvasAlignment :: Gtk.Alignment,
        coorSystemCombo :: Gtk.ComboBox,
        evalPointEntry :: Gtk.Entry,
        defaultEvalPointButton :: Gtk.Button,
        dimTable :: Gtk.Table,
        domVarLabel :: Gtk.Label,
        zoomEntry :: Gtk.Entry,
        defaultZoomPanButton :: Gtk.Button,
        centreXEntry :: Gtk.Entry,
        centreYEntry :: Gtk.Entry,
        exportPNGButton :: Gtk.Button,
        exportSVGButton :: Gtk.Button,
        exportPDFButton :: Gtk.Button,
        printTXTButton :: Gtk.Button,
        canvas :: Gtk.DrawingArea
    }

data FnViewDynWidgets = 
    FnViewDynWidgets
    {
        valueLabels :: [[Gtk.Label]]
    }

initFnViewDynWidgets :: FnViewDynWidgets
initFnViewDynWidgets =
    FnViewDynWidgets []    

    