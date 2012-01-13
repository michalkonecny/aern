module Main where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.ShowInternals

import Numeric.AERN.Misc.Debug

import qualified Graphics.UI.Gtk as Gtk

import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.List as List


--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF

main =
    do
--    Gtk.initGUI
    -- enable multithreaded GUI:
    Gtk.unsafeInitGUIForThreadedRTS
    Gtk.timeoutAddFull 
        (Concurrent.yield >> Concurrent.yield >> Concurrent.yield >> return True) 
        Gtk.priorityDefaultIdle 20
    fnDataTV <- atomically $ newTVar $ FV.FnData [[x<*>x<*>cHalf1]]
    fnMetaTV <- atomically $ newTVar $ FV.defaultFnMetaData x
--    putStrLn "plot main: calling FV.new"
    FV.new samplePoly effDrawFn eff eff (fnDataTV, fnMetaTV) Nothing
--    putStrLn "plot main: FV.new completed"
    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    
signalFn fnMetaTV =
    do
    atomically $
        do
        fnmeta <- readTVar fnMetaTV
        writeTVar fnMetaTV $ fnmeta { FV.dataFnsUpdated = True }
    
c1,c0,x,c01,cHalf,cHalf1,cOneOver16, samplePoly :: Poly
x = newProjection cfg dombox "x"
c0 = newConstFn cfg dombox 0
c1 = newConstFn cfg dombox 1
cHalf = newConstFn cfg dombox 0.5
cHalf1 = newConstFn cfg dombox $ 0.5 </\> 1
cOneOver16 = newConstFn cfg dombox $ 0.5^4
c01 = newConstFn cfg dombox $ 0 </\> 1

samplePoly = x

--eff = (100, (100,())) -- MPFR
eff = ArithInOut.roundedRealDefaultEffort (0:: CF)
minmaxEff = NumOrd.minmaxDefaultEffort x
effDrawFn = cairoDrawFnDefaultEffort samplePoly

evalOpsOutCf = evalOpsOut eff x (0::CF)

cfg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_doms = doms,
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = 4,
            ipolycfg_maxsize = 30
        }

dombox = Map.fromList $ zip vars doms

vars = ["x"]

doms :: [CF]
doms = [(0 </\> 1)]


