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
import qualified Graphics.UI.Gtk.Glade as Glade

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
    Just xml <- Glade.xmlNew gladeFileName
    window <- Glade.xmlGetWidget xml Gtk.castToWindow "window1"
    canvasAlignment <- Glade.xmlGetWidget xml Gtk.castToAlignment "canvasAlignment1"
    coorSystemCombo <- Glade.xmlGetWidget xml Gtk.castToComboBox "coorSystemCombo1"
    evalPointEntry <- Glade.xmlGetWidget xml Gtk.castToEntry "evalPointEntry1"
    defaultEvalPointButton <- Glade.xmlGetWidget xml Gtk.castToButton "defaultEvalPointButton1"
    dimTable <- Glade.xmlGetWidget xml Gtk.castToTable "dimTable1"
    domVarLabel <- Glade.xmlGetWidget xml Gtk.castToLabel "domVarLabel1"
    zoomEntry <- Glade.xmlGetWidget xml Gtk.castToEntry "zoomEntry1"
    defaultZoomPanButton <- Glade.xmlGetWidget xml Gtk.castToButton "defaultZoomPanButton1"
    centreXEntry <- Glade.xmlGetWidget xml Gtk.castToEntry "centreXEntry1"
    centreYEntry <- Glade.xmlGetWidget xml Gtk.castToEntry "centreYEntry1"
    exportJPGButton <- Glade.xmlGetWidget xml Gtk.castToButton "exportJPGButton1"
    printTXTButton <- Glade.xmlGetWidget xml Gtk.castToButton "printTXTButton1"
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
            exportJPGButton = exportJPGButton,
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
        exportJPGButton :: Gtk.Button,
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

    