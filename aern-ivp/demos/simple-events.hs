{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import Numeric.AERN.IVP.Specification.Hybrid
--import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Splitting
import Numeric.AERN.IVP.Solver.Events

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

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort


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
ivpByName "twoTanks-zenoMinus1Over16" = ivpTwoTanks_AfterZeno (-1/16) 
ivpByName "twoTanks-zeno" = ivpTwoTanks_AfterZeno 0 
ivpByName "twoTanks-zenoPlus1Over2" = ivpTwoTanks_AfterZeno 0.5 
ivpByName "twoTanks-zenoPlus2" = ivpTwoTanks_AfterZeno 2
ivpByName "twoTanksSum-zenoMinus1Over16" = ivpTwoTanksSum_AfterZeno (-1/16) 
ivpByName "twoTanksSum-zeno" = ivpTwoTanksSum_AfterZeno (0) 
ivpByName "twoTanksSum-zenoPlus1Over2" = ivpTwoTanksSum_AfterZeno 0.5 
ivpByName name = error $ "unknown IVP " ++ name


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
                Map.fromList [(eventReset, (modeAfter, switchReset))],
            hybsys_eventDetector = eventDetector
        }
    modeBefore = HybSysMode "before"
    modeAfter = HybSysMode "after"
    odeBefore, odeAfter :: [Poly] -> [Poly]
    odeBefore [x,time] = [neg x, newConstFnFromSample time (1)]
    odeAfter = odeBefore
    eventReset = HybSysEventKind "reset"
    switchReset :: [Poly] -> [Poly]
    switchReset [x,time] = [newConstFnFromSample x initValue, time]
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector (HybSysMode "after") _ = Map.empty -- reset only once!
    eventDetector _ [_x,time] =
--        let ?pCompareEffort = NumOrd.pCompareDefaultEffort x in
        case (time <? tEventPoly, tEventPoly <? time) of
            (Just True, _) -> Map.empty
            (_, Just True) -> Map.empty
            (Just False, Just False) -> Map.singleton eventReset (True, [True], id)
            _ -> Map.singleton eventReset (False, [True], id)
        where
        tEventPoly = newConstFnFromSample time $ 1 <*>| tEventDbl
    tEventDbl = 0.5 :: Double
    
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeBefore,
                    hybstate_values = [initValue, tStart]
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.singleton modeAfter,
                    hybstate_values = [xEnd, tEnd]
                }
        }
    description =
        "x' = -x; if t = " ++ show tEventDbl ++ " then x := " ++ show initValue 
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
                Map.fromList [(eventReset, (modeNormal, switchReset))],
            hybsys_eventDetector = eventDetector
        }
    modeNormal = HybSysMode "normal"
    odeNormal :: [Poly] -> [Poly]
    odeNormal [x] = [neg x]
    eventReset = HybSysEventKind "reset"
    switchReset :: [Poly] -> [Poly]
    switchReset [x] = [newConstFnFromSample x initValue]
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector _mode [x] =
--        let ?pCompareEffort = NumOrd.pCompareDefaultEffort x in
        case (xEventPoly <? x, x `leqT` xEventPoly) of
            (Just True, _) -> Map.empty -- reset ruled out
            (_, True) -> Map.singleton eventReset (True, [True], id) -- reset inevitable
            _ -> Map.singleton eventReset (False, [True], id)
        where
        xEventPoly = newConstFnFromSample x $ 1 <*>| xEventDbl
        leqT = leqOverSomeT effEval 10 tVar
        effEval = evaluationDefaultEffort x
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeNormal,
                    hybstate_values = [initValue]
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.singleton modeNormal,
                    hybstate_values = [xEnd]
                }
        }
    description =
        "x' = -x; if x <= " ++ show xEventDbl ++ " then x := " ++ show initValue 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
    initValue = 1 :: CF
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp
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
            hybsys_componentNames = ["x","x'","time"],
            hybsys_modeFields = Map.fromList [(modeBefore, odeBefore), (modeAfter, odeAfter)],
            hybsys_modeInvariants = Map.fromList [(modeBefore, id), (modeAfter, id)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeAfter, switchReset))],
            hybsys_eventDetector = eventDetector
        }
    modeBefore = HybSysMode "before"
    modeAfter = HybSysMode "after"
    odeBefore, odeAfter :: [Poly] -> [Poly]
    odeBefore [x,x',time] = [x', neg x, newConstFnFromSample time (1)]
    odeAfter = odeBefore
    eventReset = HybSysEventKind "reset"
    switchReset :: [Poly] -> [Poly]
    switchReset [x,_x',time] = map (newConstFnFromSample x) initValues ++ [time]
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector (HybSysMode "after") _ = Map.empty -- reset only once!
    eventDetector _ [_x,_x',time] =
--        let ?pCompareEffort = NumOrd.pCompareDefaultEffort x in
        case (time <? tEventPoly, tEventPoly <? time) of
            (Just True, _) -> Map.empty
            (_, Just True) -> Map.empty
            (Just False, Just False) -> Map.singleton eventReset (True, [True,True], id)
            _ -> Map.singleton eventReset (False, [True, True], id)
        where
        tEventPoly = newConstFnFromSample time $ 1 <*>| tEventDbl
    tEventDbl = 0.5 :: Double
    
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeBefore,
                    hybstate_values = initValues ++ [tStart]
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.singleton modeAfter,
                    hybstate_values = [xEnd, xDerEnd, tEnd]
                }
        }
    description =
        "x'' = -x; if t = " ++ show tEventDbl ++ " then [x,x'] := " ++ show initValues 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", x'(" ++ show tStart ++ ") = " ++ show initX'
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
            hybsys_componentNames = ["x","x'"],
            hybsys_modeFields = Map.fromList [(modeNormal, odeNormal)],
            hybsys_modeInvariants = Map.fromList [(modeNormal, id)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeNormal, switchReset))],
            hybsys_eventDetector = eventDetector
        }
    modeNormal = HybSysMode "normal"
    odeNormal :: [Poly] -> [Poly]
    odeNormal [x,x'] = [x', neg x]
    eventReset = HybSysEventKind "reset"
    switchReset :: [Poly] -> [Poly]
    switchReset [x,_x'] = map (newConstFnFromSample x) initValues
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector _mode [x,_x'] =
--        let ?pCompareEffort = NumOrd.pCompareDefaultEffort x in
        case (xEventPoly <? x, x `leqT` xEventPoly) of
            (Just True, _) -> Map.empty -- reset ruled out
            (_, True) -> Map.singleton eventReset (True, [True,True], id) -- reset inevitable
            _ -> Map.singleton eventReset (False, [True, True], id)
        where
        xEventPoly = newConstFnFromSample x $ 1 <*>| xEventDbl
        leqT = leqOverSomeT effEval 10 tVar
        effEval = evaluationDefaultEffort x
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeNormal,
                    hybstate_values = initValues ++ [tStart]
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.singleton modeNormal,
                    hybstate_values = [xEnd, xDerEnd, tEnd]
                }
        }
    description =
        "x'' = -x; if x <= " ++ show xEventDbl ++ " then [x,x'] := " ++ show initValues 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", x'(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [1,0] :: [CF]
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp
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
            hybsys_componentNames = ["x","x'"],
            hybsys_modeFields = Map.fromList [(modeFreeFall, odeFreeFall)],
            hybsys_modeInvariants = Map.fromList [(modeFreeFall, invariantFreeFall)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventBounce, (modeFreeFall, switchBounce))],
            hybsys_eventDetector = eventDetector
        }
    modeFreeFall = HybSysMode "freefall"
    odeFreeFall :: [Poly] -> [Poly]
    odeFreeFall [x,x'] = [x', newConstFnFromSample x (-10)]
    invariantFreeFall [x,x'] = [makeNonneg x,x']
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,x'] = [0, x']
    switchBounce :: [Poly] -> [Poly]
    switchBounce [x,x'] = 
        [x, (-0.5 :: Double) |<*> x']
--        [newConstFnFromSample x' 0, (0 :: Double) |<*> x']
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector _mode [x,x'] =
--        let ?pCompareEffort = NumOrd.pCompareDefaultEffort x in
        case (zP <? x, zP <? x', [x,x'] `allLeqT` 0) of
            (Just True, _, _) -> Map.empty -- ball above ground, bounce ruled out
            (_, Just True, _) -> Map.empty -- ball rising, bounce ruled out 
            (_, _, True) -> Map.singleton eventBounce (True, [True, True], pruneBounce) -- bounce inevitable
            _ -> Map.singleton eventBounce (False, [True, True], pruneBounce)
        where
        zP = newConstFnFromSample x 0
        allLeqT fns bound = predOverSomeT allLeqBound effEval 10 tVar fns
            where
            allLeqBound vals
--                | result =
--                    unsafePrint
--                    (
--                        "bothLeqBound: result = true:"
--                        ++ "\n a = " ++ show a
--                        ++ "\n b = " ++ show b
--                        ++ "\n bound = " ++ show bound
--                    ) 
--                    result
                | otherwise = result
                where
                result =
                    and $ map (\a -> ((a <=? bound) == Just True)) vals
        effEval = evaluationDefaultEffort x
    
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeFreeFall,
                    hybstate_values = initValues
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.singleton modeFreeFall,
                    hybstate_values = [xEnd, xDerEnd]
                }
        }
    description =
        "if x <= 0 && x' <= 0 then post(x) = 0, post(x') = -0.5*pre(x') else x'' = -10" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", x'(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [5,0] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp

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
            hybsys_modeFields = Map.fromList [(modeFall, odeFall), (modeRise, odeRise)],
            hybsys_modeInvariants = Map.fromList [(modeFall, invariantFall), (modeRise, invariantRise)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventBounce, (modeRise, resetBounce)), (eventPeak, (modeFall, resetPeak))],
            hybsys_eventDetector = eventDetector
        }
    modeFall = HybSysMode "fall"
    modeRise = HybSysMode "rise"
    odeFall :: [Poly] -> [Poly]
    odeFall [x,v,r] = [v, newConstFnFromSample x (-10), newConstFnFromSample r 0]
    odeRise :: [Poly] -> [Poly]
    odeRise [x,v,r] = [v, newConstFnFromSample x (-10), newConstFnFromSample r 0]
    invariantFall [x,v,r] =
--        unsafePrintReturn
--        (
--            "invariantFall:"
--            ++ "\n x = " ++ show x
--            ++ "\n v = " ++ show v
--            ++ "\n r = " ++ show r
--            ++ "\n xNN = " ++ show xNN
--            ++ "\n vNP = " ++ show vNP
--            ++ "\n rNN = " ++ show rNN
--            ++ "\n x2 = " ++ show x2
--            ++ "\n -absV = " ++ show (-absV)
--            ++ "\n result = "
--        ) 
        [xNN CF.<\/> x2, 
         vNP CF.<\/> (-absV), 
         rNN]
        {- making use of the energy conservation law: 
           (x')^2 + 2gx = r
           
           which implies 
           |x'| = sqrt(r - 2gx) 
           x = (r - (x')^2) / 2g 
        -}
        where
        rNN = makeNonneg r
        xNN = makeNonneg x
        vNP = - (makeNonneg (- v))
        absV = CF.sqrtOut $ makeNonneg $ rNN <-> (20 <*> xNN)
        x2 = (rNN <-> (makeNonneg $ vNP <*> vNP)) </> 20
    invariantRise [x,v,r] = 
--        unsafePrintReturn
--        (
--            "invariantRise:"
--            ++ "\n x = " ++ show x
--            ++ "\n v = " ++ show v
--            ++ "\n r = " ++ show r
--            ++ "\n xNN = " ++ show xNN
--            ++ "\n vNN = " ++ show vNN
--            ++ "\n rNN = " ++ show rNN
--            ++ "\n x2 = " ++ show x2
--            ++ "\n absV = " ++ show (absV)
--            ++ "\n result = "
--        ) 
        [xNN CF.<\/> x2, 
         vNN CF.<\/> absV, 
         rNN]
        {- making use of the energy conservation law: 
           (x')^2 + 2gx = r
           
           which implies 
           |x'| = sqrt(r - 2gx)
           x = (r - (x')^2) / 2g 
        -}
        where
        rNN = makeNonneg r
        xNN = makeNonneg x
        vNN = makeNonneg v
        absV = CF.sqrtOut $ makeNonneg $ rNN <-> (20 <*> xNN)
        x2 = (rNN <-> (vNN <*> vNN)) </> 20
        
    eventBounce = HybSysEventKind "bc"
    pruneBounce [_x,v,r] = [0,v,r]
    resetBounce :: [Poly] -> [Poly]
    resetBounce [x,x',r] = 
        [x, 
         (-0.5 :: Double) |<*> x', 
         (0.25 :: Double) |<*> r]
    eventPeak = HybSysEventKind "pk"
    prunePeak [x,_v,r] = [x,0,r]
    resetPeak = id
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector mode [x,v,_r] 
        | mode == modeFall =
            case (zP <? x, [x] `allLeqT` 0) of
                (Just True, _) -> Map.empty -- ball above ground, bounce ruled out
                (_, True) -> Map.singleton eventBounce (True, [True, True, True], pruneBounce) -- bounce inevitable
                _ -> Map.singleton eventBounce (False, [True, True, True], pruneBounce)
        | mode == modeRise =
            case (zP <? v, [v] `allLeqT` 0) of
                (Just True, _) -> Map.empty -- ball rising, peak ruled out
                (_, True) -> Map.singleton eventPeak (True, [False, True, False], prunePeak) -- peak inevitable
                _ -> Map.singleton eventPeak (False, [False, True, False], prunePeak)
        where
        zP = newConstFnFromSample x 0
        allLeqT fns bound = predOverSomeT allLeqBound effEval 10 tVar fns
            where
            allLeqBound vals
--                | result =
--                    unsafePrint
--                    (
--                        "bothLeqBound: result = true:"
--                        ++ "\n a = " ++ show a
--                        ++ "\n b = " ++ show b
--                        ++ "\n bound = " ++ show bound
--                    ) 
--                    result
                | otherwise = result
                where
                result =
                    and $ map (\a -> ((a <=? bound) == Just True)) vals
        effEval = evaluationDefaultEffort x
    
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeFall,
                    hybstate_values = initValues
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.fromList [modeFall, modeRise],
                    hybstate_values = [xEnd, vEnd, rEnd]
                }
        }
    description =
        "" ++ "if falling then (if x = 0 then rising, post(v) = -v/2, post(r) = r/4 else x' = v, v' = -10, r' = 0, r = (x')^2+20x, x >= 0, v <= 0, r >= 0)" 
        ++ "\n if rising  then (if v = 0 then falling else x' = v, v' = -10, r' = 0, r = (x')^2+20x, x >= 0, v >= 0, r >= 0)" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", x'(" ++ show tStart ++ ") = " ++ show initX'
        ++ ", r(" ++ show tStart ++ ") ∊ " ++ show initR
    initValues@[initX, initX', initR] = [5, 0, energyWith initX initX'] :: [CF]
--    initValues@[initX, initX'] = [0,0] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp


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
            hybsys_eventDetector = eventDetector
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
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector mode [x1,x2] =
        case mode of
            HybSysMode "fill1" ->
                case (zP <? x2, [x2] `allLeqT` 0) of
                    (Just True, _) -> Map.empty -- tank 2 not empty throughtout T, switch ruled out
                    (_, True) -> Map.singleton event1To2 (True, [True, True], prune1To2) -- switch inevitable somewhere on T
                    _ -> Map.singleton event1To2 (False, [True, True], prune1To2) -- switch cannot be ruled out nor ascertained on T
            HybSysMode "fill2" ->
                case (zP <? x1, [x1] `allLeqT` 0) of
                    (Just True, _) -> Map.empty -- tank 1 not empty throughtout T, switch ruled out
                    (_, True) -> Map.singleton event2To1 (True, [True, True], prune2To1) -- switch inevitable somewhere on T
                    _ -> Map.singleton event2To1 (False, [True, True], prune2To1) -- switch cannot be ruled out nor ascertained on T
        where
        zP = newConstFnFromSample x1 0
        allLeqT fns bound = predOverSomeT allLeqBound effEval 10 tVar fns
            where
            allLeqBound vals
--                | result =
--                    unsafePrint
--                    (
--                        "bothLeqBound: result = true:"
--                        ++ "\n a = " ++ show a
--                        ++ "\n b = " ++ show b
--                        ++ "\n bound = " ++ show bound
--                    ) 
--                    result
                | otherwise = result
                where
                result =
                    and $ map (\a -> ((a <=? bound) == Just True)) vals
        effEval = evaluationDefaultEffort x1
    
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeFill1,
                    hybstate_values = initValues
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.fromList [modeFill1, modeFill2],
                    hybstate_values = [0, 0]
                }
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
    tVar = hybivp_tVar ivp

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
                 (modeFill2, odeFill2),
                 (modeFlow, odeFlow)
                ],
            hybsys_modeInvariants = Map.fromList 
                [(modeFill1, invariant), 
                 (modeFill2, invariant),
                 (modeFlow, invariant)
                ],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [(event1To2, (modeFill2, id)), 
                     (event2To1, (modeFill1, id)),
                     (eventEmpty, (modeFlow, id))
                    ],
            hybsys_eventDetector = eventDetector
        }
    modeFill1 = HybSysMode "fill1"
    modeFill2 = HybSysMode "fill2"
    modeFlow = HybSysMode "flow"
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
    odeFlow :: [Poly] -> [Poly]
    odeFlow [_x1,_x2,_x12] =
        [newConstFnFromSample _x1 0, 
         newConstFnFromSample _x1 0,
         newConstFnFromSample _x1 0
        ]
--    invariant = id
    invariant [x1,x2,x12] =
        [x1NN CF.<\/> (makeNonneg $ x12NN - x2NN),
         x2NN CF.<\/> (makeNonneg $ x12NN - x1NN), 
         x12NN -- CF.<\/> (x1NN + x2NN)
        ]
        where
        x1NN = makeNonneg x1
        x2NN = makeNonneg x2
        x12NN = makeNonneg x12
    event1To2 = HybSysEventKind "1To2"
    event2To1 = HybSysEventKind "2To1"
    eventEmpty = HybSysEventKind "empty"
    prune1To2 [x1,_x2, x12] = [x1, 0, x12]
    prune2To1 [_x1,x2, x12] = [0, x2, x12]
    pruneEmpty _ = [0,0,0]
--    pruneEmpty [x1, x2, _x12] = [x1, x2, 0] -- only for testing, the above definition is clearly better
    eventDetector :: HybSysMode -> [Poly] -> Map.Map HybSysEventKind (Bool, [Bool], [CF] -> [CF])
    eventDetector mode [x1,x2,x12] =
        switchEvents `Map.union` emptyEvents
        where
        emptyEvents = 
--            Map.empty
            case mode of
                HybSysMode "flow" -> Map.empty
                _ ->
                    case (zP <? x12, [x12] `allLeqT` 0) of
                        (Just True, _) -> Map.empty -- sum of tank levels positive throughtout T, emptying ruled out
                        (_, True) -> Map.singleton eventEmpty (True, [True, True, True], pruneEmpty) -- emptying inevitable somewhere on T
                        _ -> Map.singleton eventEmpty (False, [True, True, True], pruneEmpty) -- emptying cannot be ruled out nor ascertained on T
        switchEvents =
            case mode of
                HybSysMode "fill1" ->
                    case (zP <? x2, [x2] `allLeqT` 0) of
                        (Just True, _) -> Map.empty -- tank 2 not empty throughtout T, switch ruled out
                        (_, True) -> Map.singleton event1To2 (True, [True, True, False], prune1To2) -- switch inevitable somewhere on T
                        _ -> Map.singleton event1To2 (False, [True, True, False], prune1To2) -- switch cannot be ruled out nor ascertained on T
                HybSysMode "fill2" ->
                    case (zP <? x1, [x1] `allLeqT` 0) of
                        (Just True, _) -> Map.empty -- tank 1 not empty throughtout T, switch ruled out
                        (_, True) -> Map.singleton event2To1 (True, [True, True, False], prune2To1) -- switch inevitable somewhere on T
                        _ -> Map.singleton event2To1 (False, [True, True, False], prune2To1) -- switch cannot be ruled out nor ascertained on T
                HybSysMode "flow" -> Map.empty
        zP = newConstFnFromSample x1 0
        allLeqT fns bound = predOverSomeT allLeqBound effEval 10 tVar fns
            where
            allLeqBound vals
--                | result =
--                    unsafePrint
--                    (
--                        "bothLeqBound: result = true:"
--                        ++ "\n a = " ++ show a
--                        ++ "\n b = " ++ show b
--                        ++ "\n bound = " ++ show bound
--                    ) 
--                    result
                | otherwise = result
                where
                result =
                    and $ map (\a -> ((a <=? bound) == Just True)) vals
        effEval = evaluationDefaultEffort x1
    
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
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeFill1,
                    hybstate_values = initValues
                },
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
        ++ "\n if x12 = 0 then flow, x1 = x2 = x12 = 0, x1' = x2' = x12 = 0"
        ++ "\n invariant x12 = x1 + x2"
        ++ "\n ; x1(" ++ show tStart ++ ") = " ++ show initX1
        ++    ", x2(" ++ show tStart ++ ") = " ++ show initX2
        ++    ", x12(" ++ show tStart ++ ") = " ++ show initX12
    initValues@[initX1, initX2, initX12] = [1,1,2] :: [CF]
    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
    tVar = hybivp_tVar ivp

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
                Just (HybridSystemUncertainState _ vecOut) ->
                    case maybeStateExact of
                        Just (HybridSystemUncertainState _ vecExact) -> 
                                computeDiff vecOut vecExact
                        _ -> show "exact solution not known"
                where
                computeDiff vecOut vecOther = 
                    removeBracks $
                    show $
                        snd $ RefOrd.getEndpointsOutWithDefaultEffort $ 
--                            foldl1 min $ -- assuming that the components are interdependent - some may be bad due to dependency errors in the projection 
                            foldl1 max $ 
                                zipWith (CF.<->) (map CF.width vecOut) (map CF.width vecOther)
        removeBracks ('<': rest1 ) =
            reverse $ removeR $ reverse rest1
            where
            removeR ('>' : rest2 ) = rest2

refinesVec :: [CF] -> [CF] -> Bool
refinesVec vec1 vec2 =
    and $ zipWith refines vec1 vec2
refines :: CF -> CF -> Bool
refines a1 a2 = 
    (a2 CF.|<=? a1) == Just True
    where
--    tolerance = 2 ^^ (-50)

solveEventsPrintSteps :: 
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState Poly), [(HybSysMode, EventInfo Poly)]))
    =>
    Bool
    ->
    Bool
    ->
    HybridIVP Poly 
    -> 
    (Int, Int, Int, Int) 
    -> 
    IO (Maybe (HybridSystemUncertainState Poly), SplittingInfo solvingInfo (solvingInfo, Maybe CF))
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
    _ <- printStepsInfo (1:: Int) splittingInfo
    putStrLn "----------  step summary: -----------------------"
    putStrLn $ "number of atomic segments = " ++ (show $ splittingInfoCountLeafs splittingInfo)
    putStrLn $ "smallest segment size: " ++ (show smallestSegSize)  
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  splitting info: --------------------------"
            putStrLn $ showSplittingInfo showSegInfo showSplitReason "" splittingInfo
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
        True -> plotEventResolution effCf componentNames splittingInfo
    return (maybeEndState, splittingInfo)
    where
    (maybeEndState, splittingInfo) =
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
        (HybridSystemUncertainState exactModeSet exactVec) = exactState
        (HybridSystemUncertainState approxModeSet approxVec) = approxState
        getError (valueIn, valueOut) =
            err
            where
            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    
    eventCount =
        aux splittingInfo
        where
        aux (SegNoSplit (_,_,modeEventInfoList)) =
            foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf . snd) modeEventInfoList
        aux (SegSplit _ left Nothing) =
            aux left
        aux (SegSplit _ left (Just right)) =
            (aux left) CF.<+> (aux right)
    (smallestSegSize, _) =
        aux tStart (tEnd CF.<-> tStart) splittingInfo
        where
        aux tPrev tSmallestSoFar (SegNoSplit (tNow,_,_)) =
            (CF.minOut tSmallestSoFar (tNow CF.<-> tPrev), tNow)
        aux tPrev tSmallestSoFar (SegSplit _ left Nothing) =
            aux tPrev tSmallestSoFar left
        aux tPrev tSmallestSoFar (SegSplit _ left (Just right)) =
            aux tPrevL tSmallestSoFarL right
            where
            (tSmallestSoFarL, tPrevL) =
                aux tPrev tSmallestSoFar left
            
    showStepInfo (n, t) =
        "step " ++ show n ++ ": t = " ++ show t
    printStepsInfo n (SegNoSplit (t, _maybeState, _modeEventInfoList)) =
        do
        putStrLn $ showStepInfo (n, t)
        return $ n + 1
    printStepsInfo n (SegSplit _ left maybeRight) =
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
                Just (HybridSystemUncertainState modeSet values) -> 
                    (show modeSet, map showValue values)
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
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState Poly), [(HybSysMode, EventInfo Poly)]))
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
     Maybe (HybridSystemUncertainState Poly)
    ,
     SplittingInfo solvingInfo (solvingInfo, Maybe CF)
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
        solveEventsTimeSplit
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
     
plotEventResolution effCF componentNames splittingInfo =
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
    (fns, fnNames) = 
        unzip $ map getFnsFromSegInfo $ splittingInfoGetLeafSegInfoSequence splittingInfo
        where
        getFnsFromSegInfo (_,_,modeEventInfos) =
            unzip $ concat $ map getFnsFromMEI modeEventInfos
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
    segs = length fnNames
    fnmeta = 
        (FV.defaultFnMetaData samplePoly)
        {
            FV.dataFnGroupNames = map ("segment " ++) (map show [1..segs]),
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map giveColours fnNames,
            FV.dataDomL = 0,
            FV.dataDomR = 4,
            FV.dataValLO = -2,
            FV.dataValHI = 2,
            FV.dataDefaultEvalPoint = 0,
            FV.dataDefaultCanvasParams =
                (FV.defaultCanvasParams (0::CF))
                {
                    FV.cnvprmCoordSystem = 
                        FV.CoordSystemLinear $ 
                            FV.Rectangle  2 (-2) 0 (4)
                    ,
                    FV.cnvprmSamplesPerUnit = 100
                }
        }
    giveColours list =
        take (length list) colours
    colours = cycle $ map snd $ zip componentNames (cycle [blue, green, red]) 
    
    _black = FV.defaultFnPlotStyle
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
    