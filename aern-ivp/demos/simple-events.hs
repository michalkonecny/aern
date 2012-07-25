{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import Numeric.AERN.IVP.Specification.Hybrid
--import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Events.Aggregate
import Numeric.AERN.IVP.Solver.Events.Bisection

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort


import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.ShowInternals

--import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map


import System.IO
import System.Environment
import System.Directory
import System.CPUTime
import System.Timeout

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import qualified Graphics.UI.Gtk as Gtk
--import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM

import Numeric.AERN.Misc.Debug
_ = unsafePrint -- stop the unused warning

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = CF.DI
type Poly = IntPoly String CF

main :: IO ()
main =
    do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case length args of
        2 -> writeCSV args
        7 -> runOnce args
        _ -> usage
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-events <ivp name> <output file name>"
    putStrLn "Usage B: simple-events <ivp name> <maxDeg> <minStepSize> <True|False-print steps?> <maxEvalSplitSize>"

ivpByName :: String -> HybridIVP Poly
ivpByName "expDec-resetOnce" = ivpExpDecay_resetTHalf
ivpByName "expDec-resetOn34" = ivpExpDecay_resetOn34
ivpByName "springMass-resetOnce" = ivpSpringMass_resetTHalf
ivpByName "springMass-resetOn34" = ivpSpringMass_resetOn34
ivpByName "bouncingBall-after1" = ivpBouncingBall_AfterBounce 1 
ivpByName "bouncingBall-after2" = ivpBouncingBall_AfterBounce 2
ivpByName "bouncingBall-after3" = ivpBouncingBall_AfterBounce 3
ivpByName "bouncingBall-after4" = ivpBouncingBall_AfterBounce 4
ivpByName "bouncingBall-after5" = ivpBouncingBall_AfterBounce 5
ivpByName "bouncingBall-after6" = ivpBouncingBall_AfterBounce 6
ivpByName "bouncingBall-after7" = ivpBouncingBall_AfterBounce 7
ivpByName "bouncingBall-after8" = ivpBouncingBall_AfterBounce 8
ivpByName "bouncingBall-after9" = ivpBouncingBall_AfterBounce 9
ivpByName "bouncingBall-after10" = ivpBouncingBall_AfterBounce 10 
ivpByName "bouncingBall-after20" = ivpBouncingBall_AfterBounce 20 
ivpByName "bouncingBall-after30" = ivpBouncingBall_AfterBounce 30 
ivpByName "bouncingBall-after40" = ivpBouncingBall_AfterBounce 40 
ivpByName "bouncingBall-zeno" = ivpBouncingBall_AfterZeno 0 
ivpByName "bouncingBall-zenoPlus1Over2" = ivpBouncingBall_AfterZeno 0.5 
ivpByName "bouncingBall-zenoPlus2" = ivpBouncingBall_AfterZeno 2
ivpByName "bouncingBallEnergy-zeno" = ivpBouncingBallEnergy_AfterZeno 0 
ivpByName "bouncingBallEnergy-zenoPlus1Over2" = ivpBouncingBallEnergy_AfterZeno 0.5 
ivpByName "bouncingBallEnergy-zenoPlus2" = ivpBouncingBallEnergy_AfterZeno 2
ivpByName "bouncingBallVibr-graze" = ivpBouncingBallVibr_AtTime 2 
ivpByName "bouncingBallDrop" = ivpBouncingBallDrop_AtTime 3 2 0 5
ivpByName "bouncingBallEnergyDrop" = ivpBouncingBallEnergyDrop_AtTime 3 2 0 5
ivpByName "twoBouncingBallsDrop" = ivpTwoBouncingBallsDrop_AtTime 30 20 25 10 45
ivpByName "twoBouncingBallsEnergyDrop" = ivpTwoBouncingBallsEnergyDrop_AtTime 30 20 25 10 45
ivpByName "bouncingSpring-4" = ivpBouncingSpring_AtTime 4 
ivpByName "twoTanks-zenoMinus1Over16" = ivpTwoTanks_AfterZeno (-1/16) 
ivpByName "twoTanks-zeno" = ivpTwoTanks_AfterZeno 0 
ivpByName "twoTanks-zenoPlus1Over2" = ivpTwoTanks_AfterZeno 0.5 
ivpByName "twoTanks-zenoPlus2" = ivpTwoTanks_AfterZeno 2
ivpByName "twoTanksSum-zenoMinus1Over16" = ivpTwoTanksSum_AfterZeno (-1/16) 
ivpByName "twoTanksSum-zeno" = ivpTwoTanksSum_AfterZeno (0) 
ivpByName "twoTanksSum-zenoPlus1Over2" = ivpTwoTanksSum_AfterZeno 0.5 
ivpByName name = error $ "unknown IVP " ++ show name


ivpExpDecay_resetTHalf :: HybridIVP Poly
ivpExpDecay_resetTHalf =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","time"],
            hybsys_modeFields = Map.fromList [(modeBefore, odeBefore), (modeAfter, odeAfter)],
            hybsys_modeInvariants = Map.fromList [(modeBefore, id), (modeAfter, id)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeAfter, resetReset))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeBefore = HybSysMode "before"
    modeAfter = HybSysMode "after"
    odeBefore, odeAfter :: [Poly] -> [Poly]
    odeBefore [x,time] = [neg x, newConstFnFromSample time (1)]
    odeAfter = odeBefore
    eventReset = HybSysEventKind "reset"
    resetReset :: [Poly] -> [Poly]
    resetReset [x,time] = [newConstFnFromSample x initValue, time]
    eventSpecMap (HybSysMode "after") = Map.empty -- reset only once!
    eventSpecMap _ =
        Map.singleton eventReset $
            ([True,True], timeDip, const (Just True), timeReset)
        where
        timeDip [_, t] = tEventP <-> t
            where
            tEventP = newConstFnFromSample t $ 1 <*>| tEventDbl
        timeReset [x,t] = [x,zP]
            where
            zP = zero t
    tEventDbl = 0.5 :: Double
    tEvent = (0 :: CF) <+>| tEventDbl
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = 1,
            hybivp_initialStateEnclosure = 
                Map.singleton modeBefore [initValue, tStart],
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeAfter [xEnd, tEnd - tEvent]
        }
    description =
        "v = -x; if t = " ++ show tEventDbl ++ " then x := " ++ show initValue 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
    initValue = 1 :: CF
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    xEnd = 1 CF.<*>| (exp (-tEndDbl+tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd

ivpExpDecay_resetOn34 :: HybridIVP Poly
ivpExpDecay_resetOn34 =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x"],
            hybsys_modeFields = Map.fromList [(modeNormal, odeNormal)],
            hybsys_modeInvariants = Map.fromList [(modeNormal, id)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeNormal, resetReset))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeNormal = HybSysMode "normal"
    odeNormal :: [Poly] -> [Poly]
    odeNormal [x] = [neg x]
    eventReset = HybSysEventKind "reset"
    resetReset :: [Poly] -> [Poly]
    resetReset [x] = [newConstFnFromSample x initValue]
    eventSpecMap _mode =
        Map.singleton eventReset $
            ([True], xDip, const (Just True), id)
        where
        xDip [x] = x <-> xEventPoly
            where
            xEventPoly = newConstFnFromSample x $ 1 <*>| xEventDbl

    xEventDbl = 0.75 :: Double
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = 1,
            hybivp_initialStateEnclosure = 
                Map.singleton modeNormal [initValue],
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeNormal [xEnd]
        }
    description =
        "v = -x; if x <= " ++ show xEventDbl ++ " then x := " ++ show initValue 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
    initValue = 1 :: CF
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp
    xEnd = 1 CF.<*>| (exp (-tEndDbl-3*(log xEventDbl)) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd


ivpSpringMass_resetTHalf :: HybridIVP Poly
ivpSpringMass_resetTHalf =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v","time"],
            hybsys_modeFields = Map.fromList [(modeBefore, odeBefore), (modeAfter, odeAfter)],
            hybsys_modeInvariants = Map.fromList [(modeBefore, id), (modeAfter, id)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeAfter, resetReset))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeBefore = HybSysMode "before"
    modeAfter = HybSysMode "after"
    odeBefore, odeAfter :: [Poly] -> [Poly]
    odeBefore [x,v,time] = [v, neg x, newConstFnFromSample time (1)]
    odeAfter = odeBefore
    eventReset = HybSysEventKind "reset"
    resetReset :: [Poly] -> [Poly]
    resetReset [x,_v,time] = map (newConstFnFromSample x) initValues ++ [time]
    eventSpecMap (HybSysMode "after") = Map.empty -- reset only once!
    eventSpecMap _ =
        Map.singleton eventReset $
            ([True,True,True], timeDip, const (Just True), timeReset)
        where
        timeDip [_, _, t] = tEventP <-> t
            where
            tEventP = newConstFnFromSample t $ 1 <*>| tEventDbl
        timeReset [x,v,t] = [x,v,zP]
            where
            zP = zero t
    tEventDbl = 0.5 :: Double
    tEvent = (0 :: CF) <+>| tEventDbl
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = 1,
            hybivp_initialStateEnclosure = 
                Map.singleton modeBefore (initValues ++ [tStart]),
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeAfter [xEnd, xDerEnd, tEnd <-> tEvent]
        }
    description =
        "x'' = -x; if t = " ++ show tEventDbl ++ " then [x,v] := " ++ show initValues 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [1,0] :: [CF]
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    xEnd = 1 CF.<*>| (cos (tEndDbl - tEventDbl) :: Double)
    xDerEnd = (-1) CF.<*>| (sin (tEndDbl - tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd

ivpSpringMass_resetOn34 :: HybridIVP Poly
ivpSpringMass_resetOn34 =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v"],
            hybsys_modeFields = Map.fromList [(modeNormal, odeNormal)],
            hybsys_modeInvariants = Map.fromList [(modeNormal, id)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeNormal, resetReset))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeNormal = HybSysMode "normal"
    odeNormal :: [Poly] -> [Poly]
    odeNormal [x,v] = [v, neg x]
    eventReset = HybSysEventKind "reset"
    resetReset :: [Poly] -> [Poly]
    resetReset [x,_v] = map (newConstFnFromSample x) initValues
    eventSpecMap _mode =
        Map.singleton eventReset $
            ([True, True], xDip, const (Just True), id)
        where
        xDip [x,_v] = x <-> xEventPoly
            where
            xEventPoly = newConstFnFromSample x $ 1 <*>| xEventDbl
    xEventDbl = 0.75 :: Double
    tEventDbl = acos xEventDbl -- 0.72273424781341...
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = 1,
            hybivp_initialStateEnclosure = 
                Map.singleton modeNormal (initValues ++ [tStart]),
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeNormal [xEnd, xDerEnd, tEnd]
        }
    description =
        "x'' = -x; if x <= " ++ show xEventDbl ++ " then [x,v] := " ++ show initValues 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [1,0] :: [CF]
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp
    xEnd = 1 CF.<*>| (cos (tEndDbl - tEventDbl) :: Double)
    xDerEnd = (-1) CF.<*>| (sin (tEndDbl - tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd

ivpBouncingBall_AfterBounce :: Int -> HybridIVP Poly
ivpBouncingBall_AfterBounce n =
    ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 1 <*>| (3*(1 - 2^^(-n)) :: Double)
    xEnd = 1 <*>| (5 * (2^^(-2*n)) :: Double)
    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero

ivpBouncingBall_AfterZeno :: CF -> HybridIVP Poly
ivpBouncingBall_AfterZeno howLong =
    ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 3 <+> howLong
    xEnd = 0
    xDerEnd = 0

ivpBouncingBall_AtTime :: CF -> [CF] -> HybridIVP Poly
ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd] =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventBounce, (modeMove, resetBounce))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x,v] = [v, newConstFnFromSample x (-10)]
--    invariantMove = id
    invariantMove [x,v] = [makeNonneg x,v]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v] = [0, neg $ makeNonneg  $ neg v]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,v] = 
        [x, (-0.5 :: Double) |<*> v]
--        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v] = x
        vNegative [_x,v] = (v <? 0)
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeMove [xEnd, xDerEnd]
        }
    description =
        "if x = 0 && v <= 0 then post(v) = -0.5*pre(v) else x'' = -10" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [5,0] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

ivpBouncingBallEnergy_AfterBounce :: Int -> HybridIVP Poly
ivpBouncingBallEnergy_AfterBounce n =
    ivpBouncingBallEnergy_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 1 <*>| (3*(1 - 2^^(-n)) :: Double)
    xEnd = 1 <*>| (5 * (2^^(-2*n)) :: Double)
    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero

ivpBouncingBallEnergy_AfterZeno :: CF -> HybridIVP Poly
ivpBouncingBallEnergy_AfterZeno howLong =
    ivpBouncingBallEnergy_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 3 <+> howLong
    xEnd = 0
    xDerEnd = 0

ivpBouncingBallEnergy_AtTime :: CF -> [CF] -> HybridIVP Poly
ivpBouncingBallEnergy_AtTime tEnd [xEnd, vEnd] =
    ivp
    where
    rEnd = energyWith xEnd vEnd
    energyWith x v = 0 CF.</\> (v * v + 20 * x)
        -- added zero so that after reset the interval refines the original (model-level hack!)  
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v","r"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventBounce, (modeMove, resetBounce))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x,v,r] = [v, newConstFnFromSample x (-10), newConstFnFromSample r 0]
    invariantMove [x,v,r] =
        [xNN CF.<\/> x2, 
         v CF.<\/> ((-absV) CF.</\> absV), 
         rNN]
        {- making use of the energy conservation law: 
           (v)^2 + 2gx = r
           
           which implies 
           |v| = sqrt(r - 2gx) 
           x = (r - (v)^2) / 2g 
        -}
        where
        rNN = makeNonneg r
        xNN = makeNonneg x
        absV = CF.sqrtOut $ makeNonneg $ rNN <-> (20 <*> xNN)
        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> 20
    eventBounce = HybSysEventKind "bc"
    pruneBounce [_x,v,r] = [0, neg $ makeNonneg $ neg v,r]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,v,r] = 
        [x, 
         (-0.5 :: Double) |<*> v, 
         (0.25 :: Double) |<*> r]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True, True], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v, _r] = x
        vNegative [_x,v,_r] = (v <? 0)
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeMove [xEnd, vEnd, rEnd]
        }
    description =
        "" ++ "if x = 0 && v <= 0 then post(v) = -v/2, post(r) = r/4 else x''= -10, r' = 0, r = v^2+20x, x >= 0, r >= 0)" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
        ++ ", r(" ++ show tStart ++ ") ∊ " ++ show initR
    initValues@[initX, initX', initR] = [5, 0, energyWith initX initX'] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

ivpBouncingBallVibr_AtTime :: CF -> HybridIVP Poly
ivpBouncingBallVibr_AtTime tEnd =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v","y","w"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventBounce, (modeMove, resetBounce))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x,v,y,w] = 
        [v, newConstFnFromSample x (-0.81056947), -- 8/pi^2 
         w, (1::Int) |<*> (neg y)]
--    invariantMove = id
    invariantMove [x,v,y,w] = [y + (makeNonneg (x-y)),v,y,w]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v,y,w] = [y, w - (makeNonneg (w-v)),y,w]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,v,y,w] = 
        [x, w <+> ((-0.5 :: Double) |<*> (v <-> w)), y, w]
--        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True, False, False], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v,y,_w] = x <-> y
        vNegative [_x,v,_y,w] = (v <-> w <? 0)

    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if x = y && v <= w then post(v) = w -0.5*(pre(v)-prec(w)) else x'' = -10, y'' = -y" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initV
        ++ "; y(" ++ show tStart ++ ") = " ++ show initY
        ++ ", w(" ++ show tStart ++ ") = " ++ show initW
    initValues@[initX, initV, initY, initW] = [0,1.2732395,0,1] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp


ivpBouncingBallDrop_AtTime :: CF -> CF -> CF -> CF -> HybridIVP Poly
ivpBouncingBallDrop_AtTime groundInit tDrop groundDrop tEnd =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v","y","tt"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [
                        (eventBounce, (modeMove, resetBounce)),
                        (eventDrop, (modeMove, resetDrop))
                    ],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x,v,_y,_tt] = 
        [v, newConstFnFromSample x (-10), 
         newConstFnFromSample x 0, 
         newConstFnFromSample x 1]
--    invariantMove = id
    invariantMove [x,v,y,tt] = [y + (makeNonneg (x-y)),v,y,tt]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v,y,tt] = [y, neg (makeNonneg (neg v)),y,tt]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,v,y,tt] = 
        [x, ((-0.5 :: Double) |<*> v), y, tt]
    eventDrop = HybSysEventKind "drop"
    pruneDrop [x,v,y,_tt] = [x,v,y,tDrop]
    resetDrop :: [Poly] -> [Poly]
    resetDrop [x,v,y,tt] = 
        [x, v, newConstFnFromSample y groundDrop, tt <+>| (1 :: Int)] -- jump tt to ensure the event is isolated (hack!!)
    eventSpecMap _mode =
        eventsBounce `Map.union` eventsDrop
        where
        eventsDrop =
            Map.singleton eventDrop $
                ([False, False, True, True], tDip, tNearDrop, pruneDrop)
            where
            tDip [_x,_v,_y,tt] = tDropP <-> tt
                where
                tDropP = newConstFnFromSample tt tDrop
            tNearDrop [_x, _v, _y, tt] = tt <? (tDrop <+> 0.5)
        eventsBounce =
            Map.singleton eventBounce $
                ([True, True, False, False], xDip, vNegative, pruneBounce)
            where
            xDip [x,_v,y,_tt] = x <-> y
            vNegative [_x,v,_y,_tt] = (v <? 0)
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if t = " ++ show tDrop ++ " then post(y) = " ++ show groundDrop 
        ++ "else (if x = y && v <= 0 then post(v) = -0.5*(pre(v)) else x'' = -10, y' = 0)" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initV
        ++ "; y(" ++ show tStart ++ ") = " ++ show initY
    initValues@[initX, initV, initY, _initTT] = [5,0,groundInit,0] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

ivpBouncingBallEnergyDrop_AtTime :: CF -> CF -> CF -> CF -> HybridIVP Poly
ivpBouncingBallEnergyDrop_AtTime groundInit tDrop groundDrop tEnd =
    ivp
    where
    energyWith x v = 0 CF.</\> (v * v + 20 * x)
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v","r","y","tt"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [
                        (eventBounce, (modeMove, resetBounce)),
                        (eventDrop, (modeMove, resetDrop))
                    ],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x,v,r,y,tt] = 
        [v, 
         newConstFnFromSample x (-10), 
         newConstFnFromSample r 0, 
         newConstFnFromSample y 0, 
         newConstFnFromSample tt 1]
--    invariantMove = id
    invariantMove [x,v,r,y,tt] = 
        [xNN CF.<\/> x2, 
         v CF.<\/> ((-absV) CF.</\> absV), 
         rNN,
         y, tt]
        {- making use of the energy conservation law: 
           (v)^2 + 2gx = r
           
           which implies 
           |v| = sqrt(r - 2gx) 
           x = (r - (v)^2) / 2g 
        -}
        where
        rNN = makeNonneg r
        xNN = y + (makeNonneg (x - y))
        absV = CF.sqrtOut $ makeNonneg $ rNN <-> (20 <*> xNN)
        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> 20
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v,r,y,tt] = [y, neg (makeNonneg (neg v)),r,y,tt]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,v,r,y,tt] = 
        [x, 
         ((-0.5 :: Double) |<*> v),
         y2g <+> ((r <-> y2g) </>| (4 :: Int)), -- Kinetic energy is scaled by 1/4  
         y, tt]
         where
         y2g = (20 :: Double) |<*> y
    eventDrop = HybSysEventKind "drop"
    pruneDrop [x,v,r,y,_tt] = [x,v,r,y,tDrop]
    resetDrop :: [Poly] -> [Poly]
    resetDrop [x,v,r,y,tt] = 
        [x, v, 
         zP </\> r, -- include 0 to create a refinement fixed point (hack!!)  
         newConstFnFromSample y groundDrop, 
         tt <+>| (1 :: Int)] -- move clock to avoid another drop event (hack!!)
        where
        zP = newConstFnFromSample r 0
    eventSpecMap _mode =
        eventsBounce `Map.union` eventsDrop
        where
        eventsDrop =
            Map.singleton eventDrop $
                ([False, False, True, True, True], tDip, tNearDrop, pruneDrop)
            where
            tDip [_x,_v,_r, _y,tt] = tDropP <-> tt
                where
                tDropP = newConstFnFromSample tt tDrop
            tNearDrop [_x, _v,_r, _y, tt] = tt <? (tDrop <+> 0.5)
        eventsBounce =
            Map.singleton eventBounce $
                ([True, True, True, False, False], xDip, vNegative, pruneBounce)
            where
            xDip [x,_v,_r,y,_tt] = x <-> y
            vNegative [_x,v,_r,_y,_tt] = (v <? 0)
   
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if t = " ++ show tDrop ++ " then post(y) = " ++ show groundDrop 
        ++ "else (if x = y && v <= 0 then post(v) = -0.5*(pre(v)) else x'' = -10, y' = 0)" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initV
        ++ ", r(" ++ show tStart ++ ") ∊ " ++ show initR
        ++ "; y(" ++ show tStart ++ ") = " ++ show initY
    initValues@[initX, initV, initR, initY, _initTT] = [5,0,energyWith initX initV,groundInit,0] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

ivpTwoBouncingBallsDrop_AtTime :: CF -> CF -> CF -> CF -> CF -> HybridIVP Poly
ivpTwoBouncingBallsDrop_AtTime groundInit tDrop1 tDrop2Pre groundDrop tEnd =
    ivp
    where
    g = 9.81 :: Double
    c = 0.8 :: Double
    tDrop2 = tDrop2Pre + 1
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x1","v1","y1","x2","v2","y2","tt"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [
                        (eventBounce1, (modeMove, resetBounce1))
                        ,
                        (eventDrop1, (modeMove, resetDrop1))
                        ,
                        (eventBounce2, (modeMove, resetBounce2))
                        ,
                        (eventDrop2, (modeMove, resetDrop2))
                    ],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x1,v1,y1,x2,v2,y2,tt] = 
        [v1, 
         newConstFnFromSample x1 (0 <+>| (-g)), 
         newConstFnFromSample y1 0,
         v2, 
         newConstFnFromSample x2 (0 <+>| (-g)), 
         newConstFnFromSample y2 0,
         newConstFnFromSample tt 1]
--    invariantMove = id
    invariantMove [x1,v1,y1,x2,v2,y2,tt] = [y1 + (makeNonneg (x1-y1)),v1,y1,y2 + (makeNonneg (x2-y2)),v2,y2,tt]
    eventBounce1 = HybSysEventKind "bounce1"
    eventBounce2 = HybSysEventKind "bounce2"
    pruneBounce1 [_x1,v1,y1,x2,v2,y2,tt] = 
        [y1, neg (makeNonneg (neg v1)),y1,
         x2,v2,y2,
         tt]
    pruneBounce2 [x1,v1,y1,_x2,v2,y2,tt] = 
        [x1,v1,y1,
         y2, neg (makeNonneg (neg v2)),y2,
         tt]
    resetBounce1 :: [Poly] -> [Poly]
    resetBounce1 [x1,v1,y1,x2,v2,y2,tt] = 
        [x1, ((-c) |<*> v1), y1, 
         x2,v2,y2,
         tt]
    resetBounce2 [x1,v1,y1,x2,v2,y2,tt] = 
        [x1,v1,y1,
         x2, ((-c) |<*> v2), y2,
         tt]
    eventDrop1 = HybSysEventKind "drop1"
    eventDrop2 = HybSysEventKind "drop2"
    pruneDrop1 [x1,v1,y1,x2,v2,y2,_tt] = [x1,v1,y1,x2,v2,y2,tDrop1]
    pruneDrop2 [x1,v1,y1,x2,v2,y2,_tt] = [x1,v1,y1,x2,v2,y2,tDrop2]
    resetDrop1 :: [Poly] -> [Poly]
    resetDrop1 [x1,v1,y1,x2,v2,y2,tt] = 
        [x1, v1, newConstFnFromSample y1 groundDrop,
         x2,v2,y2, 
         tt <+>| (1 :: Int)] -- jump tt to avoid another drop event (hack!!)
    resetDrop2 [x1,v1,y1,x2,v2,y2,tt] = 
        [x1,v1,y1,
         x2, v2, newConstFnFromSample y2 groundDrop, 
         tt <+>| (1 :: Int)] -- jump tt to avoid another drop event (hack!!)
    eventSpecMap _mode =
        Map.unions [eventsBounce1, eventsDrop1, eventsBounce2, eventsDrop2] 
        where
        eventsDrop1 = 
            Map.singleton eventDrop1
                ([False, False, True, False, False, False, True], tDip1, tNearDrop1, pruneDrop1)
            where
            tDip1 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tDrop1P <-> tt
                where
                tDrop1P = newConstFnFromSample tt tDrop1
            tNearDrop1 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tt <? (tDrop1 <+> 0.5)
        eventsDrop2 =
            Map.singleton eventDrop2
                ([False, False, False, False, False, True, True], tDip2, tNearDrop2, pruneDrop2)
            where
            tDip2 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tDrop2P <-> tt
                where
                tDrop2P = newConstFnFromSample tt tDrop2
            tNearDrop2 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tt <? (tDrop2 <+> 0.5)
        eventsBounce1 =
            Map.singleton eventBounce1 $
                ([True, True, False, False, False, False, False], 
                    x1Dip, v1Negative, pruneBounce1)
            where
            x1Dip [x1,_v1,y1,_x2,_v2,_y2,_tt] = x1 <-> y1
            v1Negative [_x1,v1,_y1,_x2,_v2,_y2,_tt] = (v1 <? 0)
        eventsBounce2 =
            Map.singleton eventBounce2 $
                ([False, False, False, True, True, False, False], 
                    x2Dip, v2Negative, pruneBounce2)
            where
            x2Dip [_x1,_v1,_y1,x2,_v2,y2,_tt] = x2 <-> y2
            v2Negative [_x1,_v1,_y1,_x2,v2,_y2,_tt] = (v2 <? 0)
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if t = " ++ show tDrop1 ++ " then post(y1) = " ++ show groundDrop 
        ++ "else (if x1 = y1 && v1 <= 0 then post(v1) = -0.5*(pre(v1)) else x1'' = -10, y1' = 0)" 
        ++ "if t = " ++ show tDrop2 ++ " then post(y2) = " ++ show groundDrop 
        ++ "else (if x2 = y2 && v2 <= 0 then post(v2) = -0.5*(pre(v2)) else x2'' = -10, y2' = 0)" 
        ++ "; x1(" ++ show tStart ++ ") = " ++ show initX1
        ++ ", v1(" ++ show tStart ++ ") = " ++ show initV1
        ++ "; y1(" ++ show tStart ++ ") = " ++ show initY1
        ++ "; x2(" ++ show tStart ++ ") = " ++ show initX2
        ++ ", v2(" ++ show tStart ++ ") = " ++ show initV2
        ++ "; y2(" ++ show tStart ++ ") = " ++ show initY2
    initValues@[initX1, initV1, initY1, initX2, initV2, initY2, _initTT] = [30,14,groundInit,30,25,groundInit,0] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp


ivpTwoBouncingBallsEnergyDrop_AtTime :: CF -> CF -> CF -> CF -> CF -> HybridIVP Poly
ivpTwoBouncingBallsEnergyDrop_AtTime groundInit tDrop1 tDrop2Pre groundDrop tEnd =
    ivp
    where
    tDrop2 = tDrop2Pre + 1
    g = 9.81 :: Double
    c = 0.8 :: Double
    energyWith x v = 0 CF.</\> (v * v + ((2 * g) |<*> x))
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x1","v1","r1","y1","x2","v2","r2","y2","tt"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [
                        (eventBounce1, (modeMove, resetBounce1)),
                        (eventDrop1, (modeMove, resetDrop1))
                        ,
                        (eventBounce2, (modeMove, resetBounce2)),
                        (eventDrop2, (modeMove, resetDrop2))
                    ],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [v1,
         newConstFnFromSample x1 (0 <+>| (-g)), 
         newConstFnFromSample r1 0, 
         newConstFnFromSample y1 0, 
         v2,
         newConstFnFromSample x2 (0 <+>| (-g)), 
         newConstFnFromSample r2 0, 
         newConstFnFromSample y2 0,
         newConstFnFromSample tt 1]
--    invariantMove = id
    invariantMove [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1NN CF.<\/> x1E, 
         v1 CF.<\/> ((-absV1) CF.</\> absV1), 
         r1NN,
         y1, 
         x2NN CF.<\/> x2E, 
         v2 CF.<\/> ((-absV2) CF.</\> absV2), 
         r2NN,
         y2, 
         tt]
        {- making use of the energy conservation law: 
           (v)^2 + 2gx = r
           
           which implies 
           |v| = sqrt(r - 2gx) 
           x = (r - (v)^2) / 2g 
        -}
        where
        r1NN = makeNonneg r1
        x1NN = y1 + (makeNonneg (x1 - y1))
        absV1 = CF.sqrtOut $ makeNonneg $ r1NN <-> ((2*g) |<*> x1NN)
        x1E = (r1NN <-> (makeNonneg $ v1 <*> v1)) </>| (2*g)
        r2NN = makeNonneg r2
        x2NN = y2 + (makeNonneg (x2 - y2))
        absV2 = CF.sqrtOut $ makeNonneg $ r2NN <-> ((2*g) |<*> x2NN)
        x2E = (r2NN <-> (makeNonneg $ v2 <*> v2)) </>| (2*g)
    eventBounce1 = HybSysEventKind "bounce1"
    eventBounce2 = HybSysEventKind "bounce2"
    pruneBounce1 [_x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [y1, neg (makeNonneg (neg v1)),r1,y1,
         x2,v2,r2,y2,
         tt]
    pruneBounce2 [x1,v1,r1,y1,_x2,v2,r2,y2,tt] = 
        [x1,v1,r1,y1,
         y2, neg (makeNonneg (neg v2)),r2,y2,
         tt]
    resetBounce1 :: [Poly] -> [Poly]
    resetBounce1 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1, 
         ((-c) |<*> v1),
         yyg1 <+> ((c*c) |<*> (r1 <-> yyg1)), -- Kinetic energy is scaled by c^2  
         y1, 
         x2,v2,r2,y2,
         tt]
         where
         yyg1 = (2*g) |<*> y1
    resetBounce2 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1,v1,r1,y1,
         x2, 
         ((-c) |<*> v2),
         yyg2 <+> ((c*c) |<*> (r2 <-> yyg2)), -- Kinetic energy is scaled by c^2  
         y2, 
         tt]
         where
         yyg2 = (2*g) |<*> y2
    eventDrop1 = HybSysEventKind "drop1"
    eventDrop2 = HybSysEventKind "drop2"
    pruneDrop1 [x1,v1,r1,y1,x2,v2,r2,y2,_tt] = 
        [x1,v1,r1,y1,x2,v2,r2,y2,tDrop1]
    pruneDrop2 [x1,v1,r1,y1,x2,v2,r2,y2,_tt] = 
        [x1,v1,r1,y1,x2,v2,r2,y2,tDrop2]
    resetDrop1 :: [Poly] -> [Poly]
    resetDrop1 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1, v1, 
         zP </\> r1, -- include 0 to create a refinement fixed point (hack!!)  
         newConstFnFromSample y1 groundDrop,
         x2,v2,r2,y2, 
         tt <+>| (1 :: Int)] -- move clock to avoid another drop event (hack!!)
        where
        zP = newConstFnFromSample tt 0
    resetDrop2 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1,v1,r1,y1, 
         x2, v2, 
         zP </\> r2, -- include 0 to create a refinement fixed point (hack!!)  
         newConstFnFromSample y2 groundDrop,
         tt <+>| (1 :: Int)] -- move clock to avoid another drop event (hack!!)
        where
        zP = newConstFnFromSample tt 0
    eventSpecMap _mode =
        Map.unions [eventsBounce1, eventsDrop1, eventsBounce2, eventsDrop2] 
        where
        eventsDrop1 = 
            Map.singleton eventDrop1
                ([False, False, True, True, False, False, False, False, True], tDip1, tNearDrop1, pruneDrop1)
            where
            tDip1 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tDrop1P <-> tt
                where
                tDrop1P = newConstFnFromSample tt tDrop1
            tNearDrop1 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tt <? (tDrop1 <+> 0.5)
        eventsDrop2 =
            Map.singleton eventDrop2
                ([False, False, False, False, False, False, True, True, True], tDip2, tNearDrop2, pruneDrop2)
            where
            tDip2 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tDrop2P <-> tt
                where
                tDrop2P = newConstFnFromSample tt tDrop2
            tNearDrop2 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tt <? (tDrop2 <+> 0.5)
        eventsBounce1 =
            Map.singleton eventBounce1 $
                ([True, True, True, False, False, False, False, False, False], 
                    x1Dip, v1Negative, pruneBounce1)
            where
            x1Dip [x1,_v1,_r1,y1,_x2,_v2,_r2,_y2,_tt] = x1 <-> y1
            v1Negative [_x1,v1,_r1,_y1,_x2,_v2,_r2,_y2,_tt] = (v1 <? 0)
        eventsBounce2 =
            Map.singleton eventBounce2 $
                ([False, False, False, False, True, True, True, False, False], 
                    x2Dip, v2Negative, pruneBounce2)
            where
            x2Dip [_x1,_v1,_r1,_y1,x2,_v2,_r2,y2,_tt] = x2 <-> y2
            v2Negative [_x1,_v1,_r1,_y1,_x2,v2,_r2,_y2,_tt] = (v2 <? 0)
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if t = " ++ show tDrop1 ++ " then post(y1) = " ++ show groundDrop 
        ++ "else (if x1 = y1 && v1 <= 0 then post(v1) = -" ++ show c ++ "*(pre(v1)) else x1'' = -" ++ show g ++ ", y1' = 0)" 
        ++ "if t = " ++ show tDrop2 ++ " then post(y2) = " ++ show groundDrop 
        ++ "else (if x2 = y2 && v2 <= 0 then post(v2) = -" ++ show c ++ "*(pre(v2)) else x2'' = -" ++ show g ++ ", y2' = 0)" 
        ++ "; x1(" ++ show tStart ++ ") = " ++ show initX1
        ++ ", v1(" ++ show tStart ++ ") = " ++ show initV1
        ++ ", r1(" ++ show tStart ++ ") ∊ " ++ show initR1
        ++ "; y1(" ++ show tStart ++ ") = " ++ show initY1
        ++ "; x2(" ++ show tStart ++ ") = " ++ show initX2
        ++ ", v2(" ++ show tStart ++ ") = " ++ show initV2
        ++ ", r2(" ++ show tStart ++ ") ∊ " ++ show initR2
        ++ "; y2(" ++ show tStart ++ ") = " ++ show initY2
    initValues@[initX1, initV1, initR1, initY1, initX2, initV2, initR2, initY2, _initTT] = 
        [30,14,energyWith initX1 initV1,groundInit,
         30,25,energyWith initX2 initV2,groundInit,
         0] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp


ivpBouncingSpring_AtTime :: CF -> HybridIVP Poly
ivpBouncingSpring_AtTime tEnd  =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v"],
            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventBounce, (modeMove, resetBounce))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeMove = HybSysMode "move"
    odeMove :: [Poly] -> [Poly]
    odeMove [x,v] = [v, (-1 :: Int) |<*> x]
    invariantMove [x,v] = [1 + (makeNonneg (x-1)), v]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v] = [1, neg $ makeNonneg $ neg v]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,v] = 
        [x, (-0.5 :: Double) |<*> v]
--        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v] = x <+>| (-1 :: Int)
        vNegative [_x,v] = (v <? 0)

    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if x = 1 && v <= 0 then post(v) = -0.5*pre(v) else x'' = -10x" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [1,1] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

ivpTwoTanks_AfterZeno :: CF -> HybridIVP Poly
ivpTwoTanks_AfterZeno tEndMinusTZeno =
    ivp
    where
    v1 = 2 :: CF
    v2 = 3 :: CF
    w = 4 :: CF
    tZeno = 2
    tEnd = tEndMinusTZeno + tZeno
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x1","x2"],
            hybsys_modeFields = Map.fromList [(modeFill1, odeFill1), (modeFill2, odeFill2)],
            hybsys_modeInvariants = Map.fromList [(modeFill1, invariant), (modeFill2, invariant)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(event1To2, (modeFill2, id)), (event2To1, (modeFill1, id))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeFill1 = HybSysMode "fill1"
    modeFill2 = HybSysMode "fill2"
    odeFill1 :: [Poly] -> [Poly]
    odeFill1 [_x1,_x2] = [newConstFnFromSample _x1 (w - v1), newConstFnFromSample _x1 (- v2)]
    odeFill2 :: [Poly] -> [Poly]
    odeFill2 [_x1,_x2] = [newConstFnFromSample _x1 (- v1), newConstFnFromSample _x1 (w - v2)]
    invariant [x1,x2] = [makeNonneg x1, makeNonneg x2]
    event1To2 = HybSysEventKind "1To2"
    event2To1 = HybSysEventKind "2To1"
    prune1To2 [x1,_x2] = [x1, 0]
    prune2To1 [_x1,x2] = [0, x2]

    eventSpecMap (HybSysMode "fill1") =
        Map.singleton event1To2 $
            ([True, True], x2Dip, const (Just True), prune1To2)
        where
        x2Dip [_x1,x2] = x2
    eventSpecMap (HybSysMode "fill2") =
        Map.singleton event2To1 $
            ([True, True], x1Dip, const (Just True), prune2To1)
        where
        x1Dip [x1,_x2] = x1

    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeFill1 initValues,
            hybivp_maybeExactStateAtTEnd = Just $
                Map.fromList
                [
                    (modeFill1, [0,0]),
                    (modeFill2, [0,0])
                ]
        }
    description =
        ""
        ++    "if fill1 then (if x2 = 0 then fill2 else x1' = 4-2, x2' =  -3)"
        ++ "\n if fill2 then (if x1 = 0 then fill1 else x1' =  -2, x2' = 4-3)"
        ++ "\n ; x1(" ++ show tStart ++ ") = " ++ show initX1
        ++    ", x2(" ++ show tStart ++ ") = " ++ show initX2
    initValues@[initX1, initX2] = [1,1] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

ivpTwoTanksSum_AfterZeno :: CF -> HybridIVP Poly
ivpTwoTanksSum_AfterZeno tEndMinusTZeno =
    ivp
    where
    v1 = 2 :: CF
    v2 = 3 :: CF
    w = 4 :: CF
    tZeno = 2
    tEnd = tEndMinusTZeno + tZeno
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x1","x2","x12"],
            hybsys_modeFields = Map.fromList 
                [(modeFill1, odeFill1), 
                 (modeFill2, odeFill2)
                ],
            hybsys_modeInvariants = Map.fromList 
                [(modeFill1, invariant), 
                 (modeFill2, invariant)
                ],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [(event1To2, (modeFill2, id)), 
                     (event2To1, (modeFill1, id))
                    ],
            hybsys_eventSpecification = eventSpecMap
        }
    modeFill1 = HybSysMode "fill1"
    modeFill2 = HybSysMode "fill2"
    odeFill1 :: [Poly] -> [Poly]
    odeFill1 [_x1,_x2,_x12] = 
        [newConstFnFromSample _x1 (w - v1), 
         newConstFnFromSample _x1 (- v2), 
         newConstFnFromSample _x1 (w - v1- v2)
        ]
    odeFill2 :: [Poly] -> [Poly]
    odeFill2 [_x1,_x2,_x12] = 
        [newConstFnFromSample _x1 (- v1), 
         newConstFnFromSample _x1 (w - v2),
         newConstFnFromSample _x1 (w - v1- v2)
        ]
--    invariant = id
    invariant [x1,x2,x12] =
        [x1NN CF.<\/> (makeNonneg $ x12NN - x2NN),
         x2NN CF.<\/> (makeNonneg $ x12NN - x1NN), 
         x12NN CF.<\/> (x1NN + x2NN)
        ]
        where
        x1NN = makeNonneg x1
        x2NN = makeNonneg x2
        x12NN = makeNonneg x12
    event1To2 = HybSysEventKind "1To2"
    event2To1 = HybSysEventKind "2To1"
    prune1To2 [x1,_x2, x12] = [x1, 0, x12]
    prune2To1 [_x1,x2, x12] = [0, x2, x12]

    eventSpecMap (HybSysMode "fill1") =
        Map.singleton event1To2 $
            ([True, True, True], x2Dip, const (Just True), prune1To2)
        where
        x2Dip [_x1,x2,_x12] = x2
    eventSpecMap (HybSysMode "fill2") =
        Map.singleton event2To1 $
            ([True, True, True], x1Dip, const (Just True), prune2To1)
        where
        x1Dip [x1,_x2,_x12] = x1


    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeFill1 initValues,
            hybivp_maybeExactStateAtTEnd =
                Nothing 
--                Just $
--                HybridSystemUncertainState 
--                {
--                    hybstate_modes = Set.fromList [modeFlow],
--                    hybstate_values = [0, 0, 0]
--                }
        }
    description =
        ""
        ++    "if fill1 then (if x2 = 0 then fill2 else x1' = 4-2, x2' =  -3)"
        ++ "\n if fill2 then (if x1 = 0 then fill1 else x1' =  -2, x2' = 4-3)"
        ++ "\n invariant x12 = x1 + x2"
        ++ "\n ; x1(" ++ show tStart ++ ") = " ++ show initX1
        ++    ", x2(" ++ show tStart ++ ") = " ++ show initX2
        ++    ", x12(" ++ show tStart ++ ") = " ++ show initX12
    initValues@[initX1, initX2, initX12] = [1,1,2] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp

{--- END OF HYBRID SYSTEM DEFINITIONS ---}

runOnce :: [String] -> IO ()
runOnce [ivpName, maxDegS, depthS, minDepthS, shouldPlotStepsS, shouldShowStepsS, maxSplitSizeS] =
    do
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    let minDepth = read minDepthS :: Int
    let maxSplitSize = read maxSplitSizeS :: Int
    let shouldShowSteps = read shouldShowStepsS :: Bool
    let shouldPlotSteps = read shouldPlotStepsS :: Bool
    _ <- solveEventsPrintSteps shouldPlotSteps shouldShowSteps ivp (maxDeg, depth, minDepth, maxSplitSize)
    return ()
    where
    ivp = ivpByName ivpName

writeCSV :: [String] -> IO ()
writeCSV [ivpName, outputFileName] =
    do
    isClash <- doesFileExist outputFileName
    case isClash of
        True -> putStrLn $ "file " ++ outputFileName ++ " exists"
        False ->
            withFile outputFileName WriteMode $ \ handle ->
                do
                hSetBuffering handle LineBuffering
                writeCSVheader handle
                mapM_ (runSolverMeasureTimeMSwriteLine handle) paramCombinations
    where
    ivp = ivpByName ivpName
    paramCombinations = 
        [(maxDegree, depth) | 
            maxDegree <- [0..10], depth <- [0,5..60]]
--            maxDegree <- [0..10], depth <- [0..5]]
    writeCSVheader handle =
        do
        hPutStrLn handle $ "ivp: " ++ description
--        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error upper bound at t=1, error at t = 1"
        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error at t = 1"
    runSolverMeasureTimeMSwriteLine handle (maxDegree, depth) =
        do
        resultsAndTimes <- mapM solveAndMeasure ([1..1] :: [Int])
        let ((result, _) : _)  = resultsAndTimes
        let averageTime = average $ map snd resultsAndTimes
        hPutStrLn handle $ makeCSVLine ((result, averageTime), (maxDegree, depth))
        where
        average list = (2 * (sum list) + n) `div` (2 * n)
            where
            n = fromIntegral $ length list
        solveAndMeasure _ =
            do
            starttime <- getCPUTime
            maybeSolverResult <- timeout (10 * oneMinuteInMicroS) $ solveEventsPrintSteps False False ivp (maxDegree, depth, minDepth, 4*maxDegree*maxDegree)
            endtime <- getCPUTime
            let solverResult = tweakSolverResult maybeSolverResult 
            return $ (solverResult, (endtime - starttime) `div` 1000000000)
            where
            tweakSolverResult (Just solverResult2) = solverResult2
            tweakSolverResult Nothing = (Nothing, undefined)
--            oneHourInMicroS = 60 * oneMinuteInMicroS
            oneMinuteInMicroS = 60 * oneSecondInMicroS
            oneSecondInMicroS = 1000000
        minDepth = 1
    
    description = hybivp_description ivp
    maybeStateExact = hybivp_maybeExactStateAtTEnd ivp
    makeCSVLine (((maybeState, _), execTimeMS), (maxDegree, depth)) =
        show maxDegree ++ "," 
        ++ show depth ++ ","
        ++ show execTimeMS ++ ","
--        ++ enclosureErrorBoundS ++ ","
        ++ enclosureErrorS
        where
        enclosureErrorS =
            case maybeState of
                Nothing -> show "no solution"
                Just stateOut ->
                    case maybeStateExact of
                        Just stateExact -> 
                                computeDiff stateOut stateExact
                        _ -> show "exact solution not known"
                where
                computeDiff stateOut stateOther = 
                    removeBracks $
                    show $ measureImprovementState sampleCf effCf stateOut stateOther
--                        snd $ RefOrd.getEndpointsOutWithDefaultEffort $ 
----                            foldl1 min $ -- assuming that the components are interdependent - some may be bad due to dependency errors in the projection 
--                            foldl1 max $ 
--                                zipWith (CF.<->) (map CF.width vecOut) (map CF.width vecOther)
        removeBracks ('<': rest1 ) =
            reverse $ removeR $ reverse rest1
            where
            removeR ('>' : rest2 ) = rest2
    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
--    effImprCf = imprecisionDefaultEffort sampleCf

refinesVec :: [CF] -> [CF] -> Bool
refinesVec vec1 vec2 =
    and $ zipWith refines vec1 vec2
refines :: CF -> CF -> Bool
refines a1 a2 = 
    (a2 CF.|<=? a1) == Just True
    where
--    tolerance = 2 ^^ (-50)

solveEventsPrintSteps :: 
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState CF), [(HybSysMode, EventInfo Poly)]))
    =>
    Bool
    ->
    Bool
    ->
    HybridIVP Poly 
    -> 
    (Int, Int, Int, Int) 
    -> 
    IO (Maybe (HybridSystemUncertainState CF), BisectionInfo solvingInfo (solvingInfo, Maybe CF))
solveEventsPrintSteps shouldPlotSteps shouldShowSteps ivp (maxdegParam, depthParam, minDepthParam, maxSplitSizeParam) =
    do
    putStrLn "---------------------------------------------------"
    putStrLn "demo of solve-VtE from (Konecny, Taha, Duracz 2012)"
    putStrLn "---------------------------------------------------"
    putStrLn $ "solving: " ++ description
    putStrLn "-------------------------------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "substSplitSizeLimit = " ++ show substSplitSizeLimit
    putStrLn $ "minimum step size = 2^{" ++ show minStepSizeExp ++ "}"
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    case maybeExactResult of
        Just exactResult ->
            do
            putStrLn "(almost) exact result = "
            putStr $ showSegInfo "   " (tEnd, Just exactResult, [])
        _ -> return ()
    putStrLn "----------  steps: ---------------------------"
    _ <- printStepsInfo (1:: Int) bisectionInfo
    putStrLn "----------  step summary: -----------------------"
    putStrLn $ "number of atomic segments = " ++ (show $ bisectionInfoCountLeafs bisectionInfo)
    putStrLn $ "smallest segment size: " ++ (show smallestSegSize)  
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  splitting info: --------------------------"
            putStrLn $ showBisectionInfo showSegInfo showSplitReason "" bisectionInfo
        False ->
            return ()
        
    putStrLn "----------  result: -----------------------------"
    putStr $ showSegInfo ">>> " (tEnd, maybeEndState, [])
    case (maybeExactResult, maybeEndState) of
        (Just exactResult, Just resultOut) ->
            putStrLn $ "error = " ++ show (getErrorState exactResult resultOut)
        _ -> return ()
    putStrLn $ "event count = " ++ show eventCount
    putStrLn "-------------------------------------------------"
    case shouldPlotSteps of
        False -> return ()
        True -> plotEnclosures effCf (2^^(-8) :: CF) "t" componentNames bisectionInfo
    return (maybeEndState, bisectionInfo)
    where
    (maybeEndState, bisectionInfo) =
        solveHybridIVP
            sizeLimits effCf substSplitSizeLimit
                delta m minStepSize maxStepSize splitImprovementThreshold
                    "t0" 
                        ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 500
    m = 20
    substSplitSizeLimit = maxSplitSizeParam -- 2^t0maxdeg
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
    maxStepSizeExp = - minDepthParam
    maxStepSize = 2^^maxStepSizeExp
    splitImprovementThreshold = 2^^(-48 :: Int)
    
    -- auxiliary:
    description = hybivp_description ivp
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    maybeExactResult = hybivp_maybeExactStateAtTEnd ivp
    hybsys = hybivp_system ivp
    componentNames = hybsys_componentNames hybsys

    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    effJoinCf = RefOrd.joinmeetDefaultEffort sampleCf
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []
            
    getErrorState exactState approxState
        | not (exactModeSet `Set.isSubsetOf` approxModeSet) =
            error $ 
                "enclosure error:"
                ++ "\n approxModeSet = " ++ show approxModeSet
                ++ "\n exactModeSet = " ++ show exactModeSet
        | not (refinesVec exactVec approxVec) = -- && refinesVec vecIn vecExact) ->
            error $ 
                "enclosure error:"
                ++ "\n approxVec = " ++ show approxVec
                ++ "\n exactVec = " ++ show exactVec
        | otherwise =
            map getError $ zip exactVec approxVec
        where
        (exactModeSet, exactVec) = getHybridStateUnion effJoinCf exactState
        (approxModeSet, approxVec) = getHybridStateUnion effJoinCf approxState
        getError (valueIn, valueOut) =
            err
            where
            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    
    eventCount =
        aux bisectionInfo
        where
        aux (BisectionNoSplit (_,_,modeEventInfoList)) 
            | null modeEventInfoList = error "solveEventsPrintSteps: BisectionNoSplit with empty modeEventInfoList" 
            | otherwise = foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf . snd) modeEventInfoList
        aux (BisectionSplit _ left Nothing) =
            aux left
        aux (BisectionSplit _ left (Just right)) =
            (aux left) CF.<+> (aux right)
    (smallestSegSize, _) =
        aux tStart (tEnd CF.<-> tStart) bisectionInfo
        where
        aux tPrev tSmallestSoFar (BisectionNoSplit (tNow,_,_)) =
            (CF.minOut tSmallestSoFar (tNow CF.<-> tPrev), tNow)
        aux tPrev tSmallestSoFar (BisectionSplit _ left Nothing) =
            aux tPrev tSmallestSoFar left
        aux tPrev tSmallestSoFar (BisectionSplit _ left (Just right)) =
            aux tPrevL tSmallestSoFarL right
            where
            (tSmallestSoFarL, tPrevL) =
                aux tPrev tSmallestSoFar left
            
    showStepInfo (n, t) =
        "step " ++ show n ++ ": t = " ++ show t
    printStepsInfo n (BisectionNoSplit (t, _maybeState, _modeEventInfoList)) =
        do
        putStrLn $ showStepInfo (n, t)
        return $ n + 1
    printStepsInfo n (BisectionSplit _ left maybeRight) =
        do
        n2 <- printStepsInfo n left
        case maybeRight of
            Just right -> printStepsInfo n2 right
            Nothing -> return $ n2 + 1
    showSegInfo indent (t, maybeState, modeEventInfoList) =
        maybeEventsCountS ++
        indent ++ "mode(" ++ show t ++ ") ∊ " ++ modesS ++ "\n" ++
        (unlines $ map (showComponent indent) $ zip componentNames valueSs)
        ++ (unlines $ map showModeEventInfo modeEventInfoList)
        where
        maybeEventsCountS =
            case modeEventInfoList of
                [] -> ""
                _ ->  indent ++ "events on this time segment: " ++ eventsS ++ "\n" 
        eventsS =
            show $
            foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf . snd) modeEventInfoList 
        showModeEventInfo (mode, eventInfo) =
            indent ++ "events assuming mode at the start of segment = " ++ show mode ++ ":\n" ++
            showEventInfo (indent ++ "  ") (show . fst) eventInfo
        showComponent indent2 (name, valueS) =
            indent2 ++ name ++ "("  ++ show t ++ ") ∊ " ++ valueS
        (modesS, valueSs) =
            case maybeState of
                Just state ->
                    (show modeSet, map showValue values)
                    where
                    (modeSet, values) = getHybridStateUnion effJoinCf state
                _ ->
                    ("<no result computed>", 
                     replicate (length componentNames) "<no result computed>")
        showValue valueOut =
            show valueOut 
--            ++ "(err<=" ++ show err ++ ")"
--            ++ "; valueIn = " ++ show valueIn
--            where
--            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
--            wOut = CF.width valueOut     
--            wIn = CF.width valueIn     
    showSplitReason indent (segInfo, (Just improvement)) =
        showSegInfo indent segInfo ++ 
        indent ++ "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason indent (segInfo, Nothing) =
        showSegInfo indent segInfo ++ 
        indent ++ "; trying to split:"

solveHybridIVP ::
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState CF), [(HybSysMode, EventInfo Poly)]))
    =>
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF ->
    Int -> 
    CF ->
    Int ->
    CF ->
    CF ->
    CF ->
    Var Poly ->
    HybridIVP Poly 
    ->
    (
     Maybe (HybridSystemUncertainState CF)
    ,
     BisectionInfo solvingInfo (solvingInfo, Maybe CF)
    )
solveHybridIVP 
        sizeLimits effCf substSplitSizeLimit
            delta m minStepSize maxStepSize splitImprovementThreshold 
                t0Var
                    hybivp
    =
    result
    where
    result =
        solveEventsBisect
            sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effCf
                delta m t0Var minStepSize maxStepSize splitImprovementThreshold
                    hybivp

    sampleCf = delta 
    
--    effSizeLims = effCf
    effCompose = (effCf, Int1To10 substSplitSizeLimit)
    effEval = (effCf, Int1To10 substSplitSizeLimit)
    effPEval = (effCf, Int1To10 substSplitSizeLimit)
    effInteg = effCf
    effAddFn = effCf
    effMultFn = effCf
--    effMultFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, (effCf, Int1To10 20)), ())


makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Poly] -> [CF] -> Poly
makeSampleWithVarsDoms maxdeg maxsize vars doms =
    newConstFn cfg dombox sampleCf
    where
    sampleCf = 0 :: CF
    domsLE = 
        map (fst . RefOrd.getEndpointsOutWithDefaultEffort) doms
    dombox = fromList $ zip vars doms 
    cfg =
        IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_domsLZ = zipWith (CF.<->) doms domsLE,
            ipolycfg_domsLE = domsLE,
            ipolycfg_sample_cf = sampleCf,
            ipolycfg_maxdeg = maxdeg,
            ipolycfg_maxsize = maxsize
        }
     
plotEnclosures :: 
    (Num (Domain f),
     Show f,
     Show (Domain f),
     CanEvaluate f,
     CairoDrawableFn f,
     HasSizeLimits f,
     RefOrd.RoundedLattice f,
     HasConstFns f,
     HasDomainBox f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f)) 
    =>
    ArithInOut.RoundedRealEffortIndicator (Domain f)
    -> Domain f
    -> Var f
    -> [String]
    -> BisectionInfo (t, t1, [(HybSysMode, EventInfo f)]) splitReason
    -> IO ()
plotEnclosures effCF plotMinSegSize tVar componentNames bisectionInfo =
    do
    Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    FV.new samplePoly effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    Gtk.mainGUI
    where
    ((samplePoly : _) : _) = fns 
    effDrawFn = cairoDrawFnDefaultEffort samplePoly
    effEval = evaluationDefaultEffort samplePoly
    (fns, fnNames, segNames) = 
        aggregateSequencesOfTinySegments fnsAndNames 
    fnsAndNames = 
        map getFnsFromSegInfo $ bisectionInfoGetLeafSegInfoSequence bisectionInfo
        where
        getFnsFromSegInfo (_,_,modeEventInfos) =
            concat $ map getFnsFromMEI modeEventInfos
        getFnsFromMEI (HybSysMode modeName, eventInfo) =
            collectFns modeName eventInfo
        collectFns namePrefix (EventNextSure (_, fnVec) eventMap) =
            (numberFnVec fnVec namePrefix) ++
            (concat $ map perEvent $ toAscList eventMap)
            where
            perEvent (HybSysEventKind eventName, subEventInfo) =
                collectFns (namePrefix ++ "!" ++ eventName) subEventInfo
        collectFns namePrefix (EventNextMaybe (_, fnVec) eventMap) =
            (numberFnVec fnVec namePrefix) ++
            (concat $ map perEvent $ toAscList eventMap)
            where
            perEvent (HybSysEventKind eventName, subEventInfo) =
                collectFns (namePrefix ++ "?" ++ eventName) subEventInfo
        collectFns namePrefix (EventFixedPoint (_, fnVec)) =
            (numberFnVec fnVec namePrefix)
        collectFns _ _ = 
            []
        numberFnVec fnVec namePrefix =
            zipWith addName fnVec componentNames
            where
            addName fn compName = (fn, namePrefix ++ "." ++ compName)
    fnmeta = 
        (FV.defaultFnMetaData samplePoly)
        {
            FV.dataFnGroupNames = segNames, -- map ("segment " ++) (map show [1..segs]),
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map giveColours fnNames,
            FV.dataDomL = 0,
            FV.dataDomR = 4,
            FV.dataValLO = -2,
            FV.dataValHI = 2,
            FV.dataDefaultActiveFns = map whichActive fnNames,
            FV.dataDefaultEvalPoint = 0,
            FV.dataDefaultCanvasParams =
                (FV.defaultCanvasParams (0::CF))
                {
                    FV.cnvprmCoordSystem = 
                        FV.CoordSystemLinear $ 
                            FV.Rectangle  2 (-2) 0 (4)
                    ,
                    FV.cnvprmSamplesPerUnit = 200
                    ,
                    FV.cnvprmBackgroundColour = Just (1,1,1,1)
                }
        }
    aggregateSequencesOfTinySegments fnsAndNames2 = aggrNewSegm [] [] [] $ zip ([1..]::[Int]) fnsAndNames2
        where
        aggrNewSegm prevFns prevFnNames prevSegNames [] =
            (reverse prevFns, reverse prevFnNames, reverse prevSegNames)
        aggrNewSegm 
                prevFns prevFnNames prevSegNames 
                segs@((segNoFirstSeg, fnsNamesFirstSeg@((fn1FirstSeg,_) : _)) : restSegs)
            | noAggregation =
                aggrNewSegm 
                    (fnsFirstSeg : prevFns) 
                    (fnNamesFirstSeg : prevFnNames) 
                    (("segment " ++ show segNoFirstSeg) : prevSegNames) 
                    restSegs 
            | otherwise =
                aggrNewSegm
                    (fnsAggregated : prevFns) 
                    (fnNamesAggregated : prevFnNames) 
                    (("segments " ++ show segNoFirstSeg ++ "-" ++ show segNoLastAggrSeg) : prevSegNames) 
                    restSegsAfterAggr
            where
            noAggregation = length smallSegmentsToAggregate <= 1
            (smallSegmentsToAggregate, restSegsAfterAggr) = 
                span segEndsBeforeLimit segs
                where
                segEndsBeforeLimit (_, ((fn1ThisSeg,_) : _)) =
                    (tEndThisSeg <=? tAggrLimit) == Just True
                    where
                    (_, tEndThisSeg) = getTVarDomEndpoints fn1ThisSeg
                    tAggrLimit = tStartFirstSeg <+> plotMinSegSize
            fnNamesAggregated =
                map (++ "(aggr)") componentNames
            fnsAggregated =
                foldl1 (zipWith (</\>)) $
                    chunksOf (length componentNames) $
                        map makeConstFnOverAggrDom $
                            concat $ map getFnsFromSeg smallSegmentsToAggregate
                where
                chunksOf _ [] = []
                chunksOf n list = firstN : (chunksOf n rest)
                    where
                    (firstN, rest) = splitAt n list
                getFnsFromSeg (_, fnsNames) = map fst fnsNames
                makeConstFnOverAggrDom fn =
                    newConstFn sizeLimitsNew domboxNew range
                    where
                    domboxNew = fromList [(tVar, aggrDom)]
                    sizeLimitsNew =
                        adjustSizeLimitsToVarsAndDombox fn [tVar] domboxNew sizeLimits
                    range = evalAtPointOutEff effEval dombox fn
                    sizeLimits = getSizeLimits fn         
                    dombox = getDomainBox fn
                    
            aggrDom = RefOrd.fromEndpointsOutWithDefaultEffort (tStartFirstSeg, tEndLastAggrSeg) 
            (tStartFirstSeg, _) = getTVarDomEndpoints fn1FirstSeg
            (_, tEndLastAggrSeg) = getTVarDomEndpoints fn1LastAggrSeg
            (segNoLastAggrSeg, ((fn1LastAggrSeg,_) : _)) = last smallSegmentsToAggregate 
            getTVarDomEndpoints fn =
                case lookupVar (getDomainBox fn) tVar of 
                    Just tDom -> RefOrd.getEndpointsOutWithDefaultEffort tDom 
            (fnsFirstSeg, fnNamesFirstSeg) = unzip fnsNamesFirstSeg
    whichActive list =
        take (length list) activityCycle 
        where
        activityCycle = cycle $ map snd $ zip componentNames $ 
            True : (repeat True) 
--            True : (repeat False) 
--            True : False : False : True : (repeat False) 
--            True : False : False : False : True : (repeat False) 
    
    giveColours list =
        take (length list) colourCycle
        where
        colourCycle = cycle $ map snd $ 
            zip componentNames 
                (cycle [blue, green, red, black])
--                (cycle [black]) 

    black = FV.defaultFnPlotStyle
    blue = FV.defaultFnPlotStyle 
        { 
            FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
            FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
        } 
    green = FV.defaultFnPlotStyle 
        { 
            FV.styleOutlineColour = Just (0.1,0.8,0.1,1), 
            FV.styleFillColour = Just (0.1,0.8,0.1,0.1) 
        } 
    red = FV.defaultFnPlotStyle 
        { 
            FV.styleOutlineColour = Just (0.8,0.1,0.1,1), 
            FV.styleFillColour = Just (0.8,0.1,0.1,0.1) 
        } 
    