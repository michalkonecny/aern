{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Examples.Hybrid.Simple
    Description :  simple examples of hybrid system IVPs
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Simple examples of hybrid system IVPs.
-}

module Numeric.AERN.IVP.Examples.Hybrid.Simple where

import Numeric.AERN.IVP.Specification.Hybrid

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort


import qualified Data.Map as Map

ivpByName :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f)
    )
    => 
    String {-^ IVP name - see source code for the list -} -> 
    f {-^ sample function of the type to be used in simulation -} -> 
    HybridIVP f
ivpByName "expDec-resetOnce" = ivpExpDecay_resetTHalf
--ivpByName "expDec-resetOn34" = ivpExpDecay_resetOn34
--ivpByName "springMass-resetOnce" = ivpSpringMass_resetTHalf
--ivpByName "springMass-resetOn34" = ivpSpringMass_resetOn34
--ivpByName "bouncingBall-after1" = ivpBouncingBall_AfterBounce 1 
--ivpByName "bouncingBall-after2" = ivpBouncingBall_AfterBounce 2
--ivpByName "bouncingBall-after3" = ivpBouncingBall_AfterBounce 3
--ivpByName "bouncingBall-after4" = ivpBouncingBall_AfterBounce 4
--ivpByName "bouncingBall-after5" = ivpBouncingBall_AfterBounce 5
--ivpByName "bouncingBall-after6" = ivpBouncingBall_AfterBounce 6
--ivpByName "bouncingBall-after7" = ivpBouncingBall_AfterBounce 7
--ivpByName "bouncingBall-after8" = ivpBouncingBall_AfterBounce 8
--ivpByName "bouncingBall-after9" = ivpBouncingBall_AfterBounce 9
--ivpByName "bouncingBall-after10" = ivpBouncingBall_AfterBounce 10 
--ivpByName "bouncingBall-after20" = ivpBouncingBall_AfterBounce 20 
--ivpByName "bouncingBall-after30" = ivpBouncingBall_AfterBounce 30 
--ivpByName "bouncingBall-after40" = ivpBouncingBall_AfterBounce 40 
--ivpByName "bouncingBall-zeno" = ivpBouncingBall_AfterZeno 0 
--ivpByName "bouncingBall-zenoPlus1Over2" = ivpBouncingBall_AfterZeno 0.5 
--ivpByName "bouncingBall-zenoPlus2" = ivpBouncingBall_AfterZeno 2
--ivpByName "bouncingBallEnergy-zeno" = ivpBouncingBallEnergy_AfterZeno 0 
--ivpByName "bouncingBallEnergy-zenoPlus1Over2" = ivpBouncingBallEnergy_AfterZeno 0.5 
--ivpByName "bouncingBallEnergy-zenoPlus2" = ivpBouncingBallEnergy_AfterZeno 2
--ivpByName "bouncingBallVibr-graze" = ivpBouncingBallVibr_AtTime 2 
--ivpByName "bouncingBallDrop" = ivpBouncingBallDrop_AtTime 3 2 0 5
--ivpByName "bouncingBallEnergyDrop" = ivpBouncingBallEnergyDrop_AtTime 3 2 0 5
--ivpByName "twoBouncingBallsDrop" = ivpTwoBouncingBallsDrop_AtTime 30 20 25 10 45
--ivpByName "twoBouncingBallsEnergyDrop" = ivpTwoBouncingBallsEnergyDrop_AtTime 30 20 25 10 45
--ivpByName "bouncingSpring-4" = ivpBouncingSpring_AtTime 4 
--ivpByName "twoTanks-zenoMinus1Over16" = ivpTwoTanks_AfterZeno (-1/16) 
--ivpByName "twoTanks-zeno" = ivpTwoTanks_AfterZeno 0 
--ivpByName "twoTanks-zenoPlus1Over2" = ivpTwoTanks_AfterZeno 0.5
--ivpByName "twoTanks-zenoPlus2" = ivpTwoTanks_AfterZeno 2
--ivpByName "twoTanksSum-zenoMinus1Over16" = ivpTwoTanksSum_AfterZeno (-1/16) 
--ivpByName "twoTanksSum-zeno" = ivpTwoTanksSum_AfterZeno (0) 
--ivpByName "twoTanksSum-zenoPlus1Over2" = ivpTwoTanksSum_AfterZeno 0.5 
ivpByName name = error $ "unknown IVP " ++ show name


ivpExpDecay_resetTHalf ::
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f)
    )
    => 
    f -> HybridIVP f
ivpExpDecay_resetTHalf (sampleFn :: f) =
    ivp
    where
--    system :: HybridSystem f
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
    odeBefore, odeAfter :: [f] -> [f]
    odeBefore [x,time] = [neg x, newConstFnFromSample time (toDom 1)]
    odeAfter = odeBefore
    eventReset = HybSysEventKind "reset"
    resetReset :: [f] -> [f]
    resetReset [x,time] = [newConstFnFromSample x initValue, time]
    eventSpecMap (HybSysMode "after") = Map.empty -- reset only once!
    eventSpecMap _ =
        Map.singleton eventReset $
            ([True,True], timeDip, const (Just True), timeReset)
        where
        timeDip [_, t] = tEventP <-> t
            where
            tEventP = newConstFnFromSample t $ (one sampleDom) <*>| tEventDbl
        timeReset [x,t] = [x,zP]
            where
            zP = zero t
    tEventDbl = 0.5 :: Double
    tEvent = (zero sampleDom) <+>| tEventDbl
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = toDom 0,
            hybivp_tEnd = toDom 1,
            hybivp_initialStateEnclosure = 
                Map.singleton modeBefore [initValue, tStart],
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeAfter [xEnd, tEnd <-> tEvent]
        }
    description =
        "v = -x; if t = " ++ show tEventDbl ++ " then x := " ++ show initValue 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
    initValue = (one sampleDom) :: Domain f
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    xEnd = (one sampleDom) <*>| (exp (-tEndDbl+tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort tEnd (0::Double)) tEnd
    toDom = dblToDom sampleDom
    sampleDom = getSampleDomValue sampleFn

--ivpExpDecay_resetOn34 :: HybridIVP f
--ivpExpDecay_resetOn34 =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x"],
--            hybsys_modeFields = Map.fromList [(modeNormal, odeNormal)],
--            hybsys_modeInvariants = Map.fromList [(modeNormal, id)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventReset, (modeNormal, resetReset))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeNormal = HybSysMode "normal"
--    odeNormal :: [f] -> [f]
--    odeNormal [x] = [neg x]
--    eventReset = HybSysEventKind "reset"
--    resetReset :: [f] -> [f]
--    resetReset [x] = [newConstFnFromSample x initValue]
--    eventSpecMap _mode =
--        Map.singleton eventReset $
--            ([True], xDip, const (Just True), id)
--        where
--        xDip [x] = x <-> xEventFn
--            where
--            xEventFn = newConstFnFromSample x $ 1 <*>| xEventDbl
--
--    xEventDbl = 0.75 :: Double
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = 1,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeNormal [initValue],
--            hybivp_maybeExactStateAtTEnd = Just $
--                Map.singleton modeNormal [xEnd]
--        }
--    description =
--        "v = -x; if x <= " ++ show xEventDbl ++ " then x := " ++ show initValue 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
--    initValue = (one sampleDom) :: Domain f
--    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--    xEnd = (one sampleDom) <*>| (exp (-tEndDbl-3*(log xEventDbl)) :: Double)
--    tEndDbl :: Double
--    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd
--    sampleDom = tStart
--
--
--ivpSpringMass_resetTHalf :: HybridIVP f
--ivpSpringMass_resetTHalf =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v","time"],
--            hybsys_modeFields = Map.fromList [(modeBefore, odeBefore), (modeAfter, odeAfter)],
--            hybsys_modeInvariants = Map.fromList [(modeBefore, id), (modeAfter, id)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventReset, (modeAfter, resetReset))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeBefore = HybSysMode "before"
--    modeAfter = HybSysMode "after"
--    odeBefore, odeAfter :: [f] -> [f]
--    odeBefore [x,v,time] = [v, neg x, newConstFnFromSample time (1)]
--    odeAfter = odeBefore
--    eventReset = HybSysEventKind "reset"
--    resetReset :: [f] -> [f]
--    resetReset [x,_v,time] = map (newConstFnFromSample x) initValues ++ [time]
--    eventSpecMap (HybSysMode "after") = Map.empty -- reset only once!
--    eventSpecMap _ =
--        Map.singleton eventReset $
--            ([True,True,True], timeDip, const (Just True), timeReset)
--        where
--        timeDip [_, _, t] = tEventP <-> t
--            where
--            tEventP = newConstFnFromSample t $ 1 <*>| tEventDbl
--        timeReset [x,v,t] = [x,v,zP]
--            where
--            zP = zero t
--    tEventDbl = 0.5 :: Double
--    tEvent = ((zero sampleDom) :: Domain f) <+>| tEventDbl
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = 1,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeBefore (initValues ++ [tStart]),
--            hybivp_maybeExactStateAtTEnd = Just $
--                Map.singleton modeAfter [xEnd, xDerEnd, tEnd <-> tEvent]
--        }
--    description =
--        "x'' = -x; if t = " ++ show tEventDbl ++ " then [x,v] := " ++ show initValues 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
--    initValues@[initX, initX'] = [1,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
--    xEnd = (one sampleDom) <*>| (cos (tEndDbl - tEventDbl) :: Double)
--    xDerEnd = (-1) <*>| (sin (tEndDbl - tEventDbl) :: Double)
--    tEndDbl :: Double
--    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd
--    sampleDom = tStart
--    
--    
--ivpSpringMass_resetOn34 :: HybridIVP f
--ivpSpringMass_resetOn34 =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v"],
--            hybsys_modeFields = Map.fromList [(modeNormal, odeNormal)],
--            hybsys_modeInvariants = Map.fromList [(modeNormal, id)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventReset, (modeNormal, resetReset))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeNormal = HybSysMode "normal"
--    odeNormal :: [f] -> [f]
--    odeNormal [x,v] = [v, neg x]
--    eventReset = HybSysEventKind "reset"
--    resetReset :: [f] -> [f]
--    resetReset [x,_v] = map (newConstFnFromSample x) initValues
--    eventSpecMap _mode =
--        Map.singleton eventReset $
--            ([True, True], xDip, const (Just True), id)
--        where
--        xDip [x,_v] = x <-> xEventFn
--            where
--            xEventFn = newConstFnFromSample x $ 1 <*>| xEventDbl
--    xEventDbl = 0.75 :: Double
--    tEventDbl = acos xEventDbl -- 0.72273424781341...
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = 1,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeNormal (initValues ++ [tStart]),
--            hybivp_maybeExactStateAtTEnd = Just $
--                Map.singleton modeNormal [xEnd, xDerEnd, tEnd]
--        }
--    description =
--        "x'' = -x; if x <= " ++ show xEventDbl ++ " then [x,v] := " ++ show initValues 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
--    initValues@[initX, initX'] = [one sampleDom,zero sampleDom] :: [Domain f]
--    tStart = hybivp_tStart ivp
--    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--    xEnd = (one sampleDom) <*>| (cos (tEndDbl - tEventDbl) :: Double)
--    xDerEnd = (-1) <*>| (sin (tEndDbl - tEventDbl) :: Double)
--    tEndDbl :: Double
--    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd
--    sampleDom = tStart
--
--ivpBouncingBall_AfterBounce :: Int -> HybridIVP f
--ivpBouncingBall_AfterBounce n =
--    ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd]
--    where
--    tEnd = 1 <*>| (3*(1 - 2^^(-n)) :: Double)
--    xEnd = 1 <*>| (5 * (2^^(-2*n)) :: Double)
--    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero
--
--ivpBouncingBall_AfterZeno :: Domain f -> HybridIVP f
--ivpBouncingBall_AfterZeno howLong =
--    ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd]
--    where
--    tEnd = 3 <+> howLong
--    xEnd = 0
--    xDerEnd = 0
--
--ivpBouncingBall_AtTime :: Domain f -> [Domain f] -> HybridIVP f
--ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd] =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventBounce, (modeMove, resetBounce))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x,v] = [v, newConstFnFromSample x (-10)]
----    invariantMove = id
--    invariantMove [x,v] = [makeNonneg x,v]
--    eventBounce = HybSysEventKind "bounce"
--    pruneBounce [_x,v] = [0, neg $ makeNonneg  $ neg v]
--    resetBounce :: [f] -> [f]
--    resetBounce [x,v] = 
--        [x, (-0.5 :: Double) |<*> v]
----        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
--    eventSpecMap _mode =
--        Map.singleton eventBounce $
--            ([True, True], xDip, vNegative, pruneBounce)
--        where
--        xDip [x,_v] = x
--        vNegative [_x,v] = (v <? 0)
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Just $
--                Map.singleton modeMove [xEnd, xDerEnd]
--        }
--    description =
--        "if x = 0 && v <= 0 then post(v) = -0.5*pre(v) else x'' = -10" 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
--    initValues@[initX, initX'] = [z <+>| (5::Int),z] :: [Domain f]
--    z = zero sampleDom
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--    sampleDom = tStart
--
--ivpBouncingBallEnergy_AfterBounce :: Int -> HybridIVP f
--ivpBouncingBallEnergy_AfterBounce n =
--    ivpBouncingBallEnergy_AtTime tEnd [xEnd, xDerEnd]
--    where
--    tEnd = 1 <*>| (3*(1 - 2^^(-n)) :: Double)
--    xEnd = 1 <*>| (5 * (2^^(-2*n)) :: Double)
--    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero
--
--ivpBouncingBallEnergy_AfterZeno :: Domain f -> HybridIVP f
--ivpBouncingBallEnergy_AfterZeno howLong =
--    ivpBouncingBallEnergy_AtTime tEnd [xEnd, xDerEnd]
--    where
--    tEnd = 3 <+> howLong
--    xEnd = 0
--    xDerEnd = 0
--
--ivpBouncingBallEnergy_AtTime :: Domain f -> [Domain f] -> HybridIVP f
--ivpBouncingBallEnergy_AtTime tEnd [xEnd, vEnd] =
--    ivp
--    where
--    rEnd = energyWith xEnd vEnd
--    energyWith x v = 0 </\> (v * v + 20 * x)
--        -- added zero so that after reset the interval refines the original (model-level hack!)  
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v","r"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventBounce, (modeMove, resetBounce))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x,v,r] = [v, newConstFnFromSample x (-10), newConstFnFromSample r 0]
--    invariantMove [x,v,r] =
--        [xNN <\/> x2, 
--         v <\/> ((-absV) </\> absV), 
--         rNN]
--        {- making use of the energy conservation law: 
--           (v)^2 + 2gx = r
--           
--           which implies 
--           |v| = sqrt(r - 2gx) 
--           x = (r - (v)^2) / 2g 
--        -}
--        where
--        rNN = makeNonneg r
--        xNN = makeNonneg x
--        absV = sqrtOut $ makeNonneg $ rNN <-> (20 <*> xNN)
--        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> 20
--    eventBounce = HybSysEventKind "bc"
--    pruneBounce [_x,v,r] = [0, neg $ makeNonneg $ neg v,r]
--    resetBounce :: [f] -> [f]
--    resetBounce [x,v,r] = 
--        [x, 
--         (-0.5 :: Double) |<*> v, 
--         (0.25 :: Double) |<*> r]
--    eventSpecMap _mode =
--        Map.singleton eventBounce $
--            ([True, True, True], xDip, vNegative, pruneBounce)
--        where
--        xDip [x,_v, _r] = x
--        vNegative [_x,v,_r] = (v <? 0)
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Just $
--                Map.singleton modeMove [xEnd, vEnd, rEnd]
--        }
--    description =
--        "" ++ "if x = 0 && v <= 0 then post(v) = -v/2, post(r) = r/4 else x''= -10, r' = 0, r = v^2+20x, x >= 0, r >= 0)" 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
--        ++ ", r(" ++ show tStart ++ ") ∊ " ++ show initR
--    initValues@[initX, initX', initR] = [5, 0, energyWith initX initX'] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--ivpBouncingBallVibr_AtTime :: Domain f -> HybridIVP f
--ivpBouncingBallVibr_AtTime tEnd =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v","y","w"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventBounce, (modeMove, resetBounce))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x,v,y,w] = 
--        [v, newConstFnFromSample x (-0.81056947), -- 8/pi^2 
--         w, (1::Int) |<*> (neg y)]
----    invariantMove = id
--    invariantMove [x,v,y,w] = [y + (makeNonneg (x-y)),v,y,w]
--    eventBounce = HybSysEventKind "bounce"
--    pruneBounce [_x,v,y,w] = [y, w - (makeNonneg (w-v)),y,w]
--    resetBounce :: [f] -> [f]
--    resetBounce [x,v,y,w] = 
--        [x, w <+> ((-0.5 :: Double) |<*> (v <-> w)), y, w]
----        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
--    eventSpecMap _mode =
--        Map.singleton eventBounce $
--            ([True, True, False, False], xDip, vNegative, pruneBounce)
--        where
--        xDip [x,_v,y,_w] = x <-> y
--        vNegative [_x,v,_y,w] = (v <-> w <? 0)
--
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Nothing
--        }
--    description =
--        "if x = y && v <= w then post(v) = w -0.5*(pre(v)-prec(w)) else x'' = -10, y'' = -y" 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initV
--        ++ "; y(" ++ show tStart ++ ") = " ++ show initY
--        ++ ", w(" ++ show tStart ++ ") = " ++ show initW
--    initValues@[initX, initV, initY, initW] = [0,1.2732395,0,1] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--
--ivpBouncingBallDrop_AtTime :: Domain f -> Domain f -> Domain f -> Domain f -> HybridIVP f
--ivpBouncingBallDrop_AtTime groundInit tDrop groundDrop tEnd =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v","y","tt"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList 
--                    [
--                        (eventBounce, (modeMove, resetBounce)),
--                        (eventDrop, (modeMove, resetDrop))
--                    ],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x,v,_y,_tt] = 
--        [v, newConstFnFromSample x (-10), 
--         newConstFnFromSample x 0, 
--         newConstFnFromSample x 1]
----    invariantMove = id
--    invariantMove [x,v,y,tt] = [y + (makeNonneg (x-y)),v,y,tt]
--    eventBounce = HybSysEventKind "bounce"
--    pruneBounce [_x,v,y,tt] = [y, neg (makeNonneg (neg v)),y,tt]
--    resetBounce :: [f] -> [f]
--    resetBounce [x,v,y,tt] = 
--        [x, ((-0.5 :: Double) |<*> v), y, tt]
--    eventDrop = HybSysEventKind "drop"
--    pruneDrop [x,v,y,_tt] = [x,v,y,tDrop]
--    resetDrop :: [f] -> [f]
--    resetDrop [x,v,y,tt] = 
--        [x, v, newConstFnFromSample y groundDrop, tt <+>| (1 :: Int)] -- jump tt to ensure the event is isolated (hack!!)
--    eventSpecMap _mode =
--        eventsBounce `Map.union` eventsDrop
--        where
--        eventsDrop =
--            Map.singleton eventDrop $
--                ([False, False, True, True], tDip, tNearDrop, pruneDrop)
--            where
--            tDip [_x,_v,_y,tt] = tDropP <-> tt
--                where
--                tDropP = newConstFnFromSample tt tDrop
--            tNearDrop [_x, _v, _y, tt] = tt <? (tDrop <+> 0.5)
--        eventsBounce =
--            Map.singleton eventBounce $
--                ([True, True, False, False], xDip, vNegative, pruneBounce)
--            where
--            xDip [x,_v,y,_tt] = x <-> y
--            vNegative [_x,v,_y,_tt] = (v <? 0)
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Nothing
--        }
--    description =
--        "if t = " ++ show tDrop ++ " then post(y) = " ++ show groundDrop 
--        ++ "else (if x = y && v <= 0 then post(v) = -0.5*(pre(v)) else x'' = -10, y' = 0)" 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initV
--        ++ "; y(" ++ show tStart ++ ") = " ++ show initY
--    initValues@[initX, initV, initY, _initTT] = [5,0,groundInit,0] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--ivpBouncingBallEnergyDrop_AtTime :: Domain f -> Domain f -> Domain f -> Domain f -> HybridIVP f
--ivpBouncingBallEnergyDrop_AtTime groundInit tDrop groundDrop tEnd =
--    ivp
--    where
--    energyWith x v = 0 </\> (v * v + 20 * x)
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v","r","y","tt"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList 
--                    [
--                        (eventBounce, (modeMove, resetBounce)),
--                        (eventDrop, (modeMove, resetDrop))
--                    ],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x,v,r,y,tt] = 
--        [v, 
--         newConstFnFromSample x (-10), 
--         newConstFnFromSample r 0, 
--         newConstFnFromSample y 0, 
--         newConstFnFromSample tt 1]
----    invariantMove = id
--    invariantMove [x,v,r,y,tt] = 
--        [xNN <\/> x2, 
--         v <\/> ((-absV) </\> absV), 
--         rNN,
--         y, tt]
--        {- making use of the energy conservation law: 
--           (v)^2 + 2gx = r
--           
--           which implies 
--           |v| = sqrt(r - 2gx) 
--           x = (r - (v)^2) / 2g 
--        -}
--        where
--        rNN = makeNonneg r
--        xNN = y + (makeNonneg (x - y))
--        absV = sqrtOut $ makeNonneg $ rNN <-> (20 <*> xNN)
--        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> 20
--    eventBounce = HybSysEventKind "bounce"
--    pruneBounce [_x,v,r,y,tt] = [y, neg (makeNonneg (neg v)),r,y,tt]
--    resetBounce :: [f] -> [f]
--    resetBounce [x,v,r,y,tt] = 
--        [x, 
--         ((-0.5 :: Double) |<*> v),
--         y2g <+> ((r <-> y2g) </>| (4 :: Int)), -- Kinetic energy is scaled by 1/4  
--         y, tt]
--         where
--         y2g = (20 :: Double) |<*> y
--    eventDrop = HybSysEventKind "drop"
--    pruneDrop [x,v,r,y,_tt] = [x,v,r,y,tDrop]
--    resetDrop :: [f] -> [f]
--    resetDrop [x,v,r,y,tt] = 
--        [x, v, 
--         zP </\> r, -- include 0 to create a refinement fixed point (hack!!)  
--         newConstFnFromSample y groundDrop, 
--         tt <+>| (1 :: Int)] -- move clock to avoid another drop event (hack!!)
--        where
--        zP = newConstFnFromSample r 0
--    eventSpecMap _mode =
--        eventsBounce `Map.union` eventsDrop
--        where
--        eventsDrop =
--            Map.singleton eventDrop $
--                ([False, False, True, True, True], tDip, tNearDrop, pruneDrop)
--            where
--            tDip [_x,_v,_r, _y,tt] = tDropP <-> tt
--                where
--                tDropP = newConstFnFromSample tt tDrop
--            tNearDrop [_x, _v,_r, _y, tt] = tt <? (tDrop <+> 0.5)
--        eventsBounce =
--            Map.singleton eventBounce $
--                ([True, True, True, False, False], xDip, vNegative, pruneBounce)
--            where
--            xDip [x,_v,_r,y,_tt] = x <-> y
--            vNegative [_x,v,_r,_y,_tt] = (v <? 0)
--   
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Nothing
--        }
--    description =
--        "if t = " ++ show tDrop ++ " then post(y) = " ++ show groundDrop 
--        ++ "else (if x = y && v <= 0 then post(v) = -0.5*(pre(v)) else x'' = -10, y' = 0)" 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initV
--        ++ ", r(" ++ show tStart ++ ") ∊ " ++ show initR
--        ++ "; y(" ++ show tStart ++ ") = " ++ show initY
--    initValues@[initX, initV, initR, initY, _initTT] = [5,0,energyWith initX initV,groundInit,0] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--ivpTwoBouncingBallsDrop_AtTime :: Domain f -> Domain f -> Domain f -> Domain f -> Domain f -> HybridIVP f
--ivpTwoBouncingBallsDrop_AtTime groundInit tDrop1 tDrop2Pre groundDrop tEnd =
--    ivp
--    where
--    g = 9.81 :: Double
--    c = 0.8 :: Double
--    tDrop2 = tDrop2Pre + 1
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x1","v1","y1","x2","v2","y2","tt"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList 
--                    [
--                        (eventBounce1, (modeMove, resetBounce1))
--                        ,
--                        (eventDrop1, (modeMove, resetDrop1))
--                        ,
--                        (eventBounce2, (modeMove, resetBounce2))
--                        ,
--                        (eventDrop2, (modeMove, resetDrop2))
--                    ],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x1,v1,y1,x2,v2,y2,tt] = 
--        [v1, 
--         newConstFnFromSample x1 (0 <+>| (-g)), 
--         newConstFnFromSample y1 0,
--         v2, 
--         newConstFnFromSample x2 (0 <+>| (-g)), 
--         newConstFnFromSample y2 0,
--         newConstFnFromSample tt 1]
----    invariantMove = id
--    invariantMove [x1,v1,y1,x2,v2,y2,tt] = [y1 + (makeNonneg (x1-y1)),v1,y1,y2 + (makeNonneg (x2-y2)),v2,y2,tt]
--    eventBounce1 = HybSysEventKind "bounce1"
--    eventBounce2 = HybSysEventKind "bounce2"
--    pruneBounce1 [_x1,v1,y1,x2,v2,y2,tt] = 
--        [y1, neg (makeNonneg (neg v1)),y1,
--         x2,v2,y2,
--         tt]
--    pruneBounce2 [x1,v1,y1,_x2,v2,y2,tt] = 
--        [x1,v1,y1,
--         y2, neg (makeNonneg (neg v2)),y2,
--         tt]
--    resetBounce1 :: [f] -> [f]
--    resetBounce1 [x1,v1,y1,x2,v2,y2,tt] = 
--        [x1, ((-c) |<*> v1), y1, 
--         x2,v2,y2,
--         tt]
--    resetBounce2 [x1,v1,y1,x2,v2,y2,tt] = 
--        [x1,v1,y1,
--         x2, ((-c) |<*> v2), y2,
--         tt]
--    eventDrop1 = HybSysEventKind "drop1"
--    eventDrop2 = HybSysEventKind "drop2"
--    pruneDrop1 [x1,v1,y1,x2,v2,y2,_tt] = [x1,v1,y1,x2,v2,y2,tDrop1]
--    pruneDrop2 [x1,v1,y1,x2,v2,y2,_tt] = [x1,v1,y1,x2,v2,y2,tDrop2]
--    resetDrop1 :: [f] -> [f]
--    resetDrop1 [x1,v1,y1,x2,v2,y2,tt] = 
--        [x1, v1, newConstFnFromSample y1 groundDrop,
--         x2,v2,y2, 
--         tt <+>| (1 :: Int)] -- jump tt to avoid another drop event (hack!!)
--    resetDrop2 [x1,v1,y1,x2,v2,y2,tt] = 
--        [x1,v1,y1,
--         x2, v2, newConstFnFromSample y2 groundDrop, 
--         tt <+>| (1 :: Int)] -- jump tt to avoid another drop event (hack!!)
--    eventSpecMap _mode =
--        Map.unions [eventsBounce1, eventsDrop1, eventsBounce2, eventsDrop2] 
--        where
--        eventsDrop1 = 
--            Map.singleton eventDrop1
--                ([False, False, True, False, False, False, True], tDip1, tNearDrop1, pruneDrop1)
--            where
--            tDip1 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tDrop1P <-> tt
--                where
--                tDrop1P = newConstFnFromSample tt tDrop1
--            tNearDrop1 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tt <? (tDrop1 <+> 0.5)
--        eventsDrop2 =
--            Map.singleton eventDrop2
--                ([False, False, False, False, False, True, True], tDip2, tNearDrop2, pruneDrop2)
--            where
--            tDip2 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tDrop2P <-> tt
--                where
--                tDrop2P = newConstFnFromSample tt tDrop2
--            tNearDrop2 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tt <? (tDrop2 <+> 0.5)
--        eventsBounce1 =
--            Map.singleton eventBounce1 $
--                ([True, True, False, False, False, False, False], 
--                    x1Dip, v1Negative, pruneBounce1)
--            where
--            x1Dip [x1,_v1,y1,_x2,_v2,_y2,_tt] = x1 <-> y1
--            v1Negative [_x1,v1,_y1,_x2,_v2,_y2,_tt] = (v1 <? 0)
--        eventsBounce2 =
--            Map.singleton eventBounce2 $
--                ([False, False, False, True, True, False, False], 
--                    x2Dip, v2Negative, pruneBounce2)
--            where
--            x2Dip [_x1,_v1,_y1,x2,_v2,y2,_tt] = x2 <-> y2
--            v2Negative [_x1,_v1,_y1,_x2,v2,_y2,_tt] = (v2 <? 0)
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Nothing
--        }
--    description =
--        "if t = " ++ show tDrop1 ++ " then post(y1) = " ++ show groundDrop 
--        ++ "else (if x1 = y1 && v1 <= 0 then post(v1) = -0.5*(pre(v1)) else x1'' = -10, y1' = 0)" 
--        ++ "if t = " ++ show tDrop2 ++ " then post(y2) = " ++ show groundDrop 
--        ++ "else (if x2 = y2 && v2 <= 0 then post(v2) = -0.5*(pre(v2)) else x2'' = -10, y2' = 0)" 
--        ++ "; x1(" ++ show tStart ++ ") = " ++ show initX1
--        ++ ", v1(" ++ show tStart ++ ") = " ++ show initV1
--        ++ "; y1(" ++ show tStart ++ ") = " ++ show initY1
--        ++ "; x2(" ++ show tStart ++ ") = " ++ show initX2
--        ++ ", v2(" ++ show tStart ++ ") = " ++ show initV2
--        ++ "; y2(" ++ show tStart ++ ") = " ++ show initY2
--    initValues@[initX1, initV1, initY1, initX2, initV2, initY2, _initTT] = [30,14,groundInit,30,25,groundInit,0] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--
--ivpTwoBouncingBallsEnergyDrop_AtTime :: Domain f -> Domain f -> Domain f -> Domain f -> Domain f -> HybridIVP f
--ivpTwoBouncingBallsEnergyDrop_AtTime groundInit tDrop1 tDrop2Pre groundDrop tEnd =
--    ivp
--    where
--    tDrop2 = tDrop2Pre + 1
--    g = 9.81 :: Double
--    c = 0.8 :: Double
--    energyWith x v = 0 </\> (v * v + ((2 * g) |<*> x))
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x1","v1","r1","y1","x2","v2","r2","y2","tt"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList 
--                    [
--                        (eventBounce1, (modeMove, resetBounce1)),
--                        (eventDrop1, (modeMove, resetDrop1))
--                        ,
--                        (eventBounce2, (modeMove, resetBounce2)),
--                        (eventDrop2, (modeMove, resetDrop2))
--                    ],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [v1,
--         newConstFnFromSample x1 (0 <+>| (-g)), 
--         newConstFnFromSample r1 0, 
--         newConstFnFromSample y1 0, 
--         v2,
--         newConstFnFromSample x2 (0 <+>| (-g)), 
--         newConstFnFromSample r2 0, 
--         newConstFnFromSample y2 0,
--         newConstFnFromSample tt 1]
----    invariantMove = id
--    invariantMove [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [x1NN <\/> x1E, 
--         v1 <\/> ((-absV1) </\> absV1), 
--         r1NN,
--         y1, 
--         x2NN <\/> x2E, 
--         v2 <\/> ((-absV2) </\> absV2), 
--         r2NN,
--         y2, 
--         tt]
--        {- making use of the energy conservation law: 
--           (v)^2 + 2gx = r
--           
--           which implies 
--           |v| = sqrt(r - 2gx) 
--           x = (r - (v)^2) / 2g 
--        -}
--        where
--        r1NN = makeNonneg r1
--        x1NN = y1 + (makeNonneg (x1 - y1))
--        absV1 = sqrtOut $ makeNonneg $ r1NN <-> ((2*g) |<*> x1NN)
--        x1E = (r1NN <-> (makeNonneg $ v1 <*> v1)) </>| (2*g)
--        r2NN = makeNonneg r2
--        x2NN = y2 + (makeNonneg (x2 - y2))
--        absV2 = sqrtOut $ makeNonneg $ r2NN <-> ((2*g) |<*> x2NN)
--        x2E = (r2NN <-> (makeNonneg $ v2 <*> v2)) </>| (2*g)
--    eventBounce1 = HybSysEventKind "bounce1"
--    eventBounce2 = HybSysEventKind "bounce2"
--    pruneBounce1 [_x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [y1, neg (makeNonneg (neg v1)),r1,y1,
--         x2,v2,r2,y2,
--         tt]
--    pruneBounce2 [x1,v1,r1,y1,_x2,v2,r2,y2,tt] = 
--        [x1,v1,r1,y1,
--         y2, neg (makeNonneg (neg v2)),r2,y2,
--         tt]
--    resetBounce1 :: [f] -> [f]
--    resetBounce1 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [x1, 
--         ((-c) |<*> v1),
--         yyg1 <+> ((c*c) |<*> (r1 <-> yyg1)), -- Kinetic energy is scaled by c^2  
--         y1, 
--         x2,v2,r2,y2,
--         tt]
--         where
--         yyg1 = (2*g) |<*> y1
--    resetBounce2 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [x1,v1,r1,y1,
--         x2, 
--         ((-c) |<*> v2),
--         yyg2 <+> ((c*c) |<*> (r2 <-> yyg2)), -- Kinetic energy is scaled by c^2  
--         y2, 
--         tt]
--         where
--         yyg2 = (2*g) |<*> y2
--    eventDrop1 = HybSysEventKind "drop1"
--    eventDrop2 = HybSysEventKind "drop2"
--    pruneDrop1 [x1,v1,r1,y1,x2,v2,r2,y2,_tt] = 
--        [x1,v1,r1,y1,x2,v2,r2,y2,tDrop1]
--    pruneDrop2 [x1,v1,r1,y1,x2,v2,r2,y2,_tt] = 
--        [x1,v1,r1,y1,x2,v2,r2,y2,tDrop2]
--    resetDrop1 :: [f] -> [f]
--    resetDrop1 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [x1, v1, 
--         zP </\> r1, -- include 0 to create a refinement fixed point (hack!!)  
--         newConstFnFromSample y1 groundDrop,
--         x2,v2,r2,y2, 
--         tt <+>| (1 :: Int)] -- move clock to avoid another drop event (hack!!)
--        where
--        zP = newConstFnFromSample tt 0
--    resetDrop2 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
--        [x1,v1,r1,y1, 
--         x2, v2, 
--         zP </\> r2, -- include 0 to create a refinement fixed point (hack!!)  
--         newConstFnFromSample y2 groundDrop,
--         tt <+>| (1 :: Int)] -- move clock to avoid another drop event (hack!!)
--        where
--        zP = newConstFnFromSample tt 0
--    eventSpecMap _mode =
--        Map.unions [eventsBounce1, eventsDrop1, eventsBounce2, eventsDrop2] 
--        where
--        eventsDrop1 = 
--            Map.singleton eventDrop1
--                ([False, False, True, True, False, False, False, False, True], tDip1, tNearDrop1, pruneDrop1)
--            where
--            tDip1 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tDrop1P <-> tt
--                where
--                tDrop1P = newConstFnFromSample tt tDrop1
--            tNearDrop1 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tt <? (tDrop1 <+> 0.5)
--        eventsDrop2 =
--            Map.singleton eventDrop2
--                ([False, False, False, False, False, False, True, True, True], tDip2, tNearDrop2, pruneDrop2)
--            where
--            tDip2 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tDrop2P <-> tt
--                where
--                tDrop2P = newConstFnFromSample tt tDrop2
--            tNearDrop2 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tt <? (tDrop2 <+> 0.5)
--        eventsBounce1 =
--            Map.singleton eventBounce1 $
--                ([True, True, True, False, False, False, False, False, False], 
--                    x1Dip, v1Negative, pruneBounce1)
--            where
--            x1Dip [x1,_v1,_r1,y1,_x2,_v2,_r2,_y2,_tt] = x1 <-> y1
--            v1Negative [_x1,v1,_r1,_y1,_x2,_v2,_r2,_y2,_tt] = (v1 <? 0)
--        eventsBounce2 =
--            Map.singleton eventBounce2 $
--                ([False, False, False, False, True, True, True, False, False], 
--                    x2Dip, v2Negative, pruneBounce2)
--            where
--            x2Dip [_x1,_v1,_r1,_y1,x2,_v2,_r2,y2,_tt] = x2 <-> y2
--            v2Negative [_x1,_v1,_r1,_y1,_x2,v2,_r2,_y2,_tt] = (v2 <? 0)
--    
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Nothing
--        }
--    description =
--        "if t = " ++ show tDrop1 ++ " then post(y1) = " ++ show groundDrop 
--        ++ "else (if x1 = y1 && v1 <= 0 then post(v1) = -" ++ show c ++ "*(pre(v1)) else x1'' = -" ++ show g ++ ", y1' = 0)" 
--        ++ "if t = " ++ show tDrop2 ++ " then post(y2) = " ++ show groundDrop 
--        ++ "else (if x2 = y2 && v2 <= 0 then post(v2) = -" ++ show c ++ "*(pre(v2)) else x2'' = -" ++ show g ++ ", y2' = 0)" 
--        ++ "; x1(" ++ show tStart ++ ") = " ++ show initX1
--        ++ ", v1(" ++ show tStart ++ ") = " ++ show initV1
--        ++ ", r1(" ++ show tStart ++ ") ∊ " ++ show initR1
--        ++ "; y1(" ++ show tStart ++ ") = " ++ show initY1
--        ++ "; x2(" ++ show tStart ++ ") = " ++ show initX2
--        ++ ", v2(" ++ show tStart ++ ") = " ++ show initV2
--        ++ ", r2(" ++ show tStart ++ ") ∊ " ++ show initR2
--        ++ "; y2(" ++ show tStart ++ ") = " ++ show initY2
--    initValues@[initX1, initV1, initR1, initY1, initX2, initV2, initR2, initY2, _initTT] = 
--        [30,14,energyWith initX1 initV1,groundInit,
--         30,25,energyWith initX2 initV2,groundInit,
--         0] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--
--ivpBouncingSpring_AtTime :: Domain f -> HybridIVP f
--ivpBouncingSpring_AtTime tEnd  =
--    ivp
--    where
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x","v"],
--            hybsys_modeFields = Map.fromList [(modeMove, odeMove)],
--            hybsys_modeInvariants = Map.fromList [(modeMove, invariantMove)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(eventBounce, (modeMove, resetBounce))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeMove = HybSysMode "move"
--    odeMove :: [f] -> [f]
--    odeMove [x,v] = [v, (-1 :: Int) |<*> x]
--    invariantMove [x,v] = [1 + (makeNonneg (x-1)), v]
--    eventBounce = HybSysEventKind "bounce"
--    pruneBounce [_x,v] = [1, neg $ makeNonneg $ neg v]
--    resetBounce :: [f] -> [f]
--    resetBounce [x,v] = 
--        [x, (-0.5 :: Double) |<*> v]
----        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
--    eventSpecMap _mode =
--        Map.singleton eventBounce $
--            ([True, True], xDip, vNegative, pruneBounce)
--        where
--        xDip [x,_v] = x <+>| (-1 :: Int)
--        vNegative [_x,v] = (v <? 0)
--
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeMove initValues,
--            hybivp_maybeExactStateAtTEnd = Nothing
--        }
--    description =
--        "if x = 1 && v <= 0 then post(v) = -0.5*pre(v) else x'' = -10x" 
--        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
--        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
--    initValues@[initX, initX'] = [1,1] :: [Domain f]
----    initValues@[initX, initX'] = [0,0] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--ivpTwoTanks_AfterZeno :: Domain f -> HybridIVP f
--ivpTwoTanks_AfterZeno tEndMinusTZeno =
--    ivp
--    where
--    v1 = 2 :: Domain f
--    v2 = 3 :: Domain f
--    w = 4 :: Domain f
--    tZeno = 2
--    tEnd = tEndMinusTZeno + tZeno
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x1","x2"],
--            hybsys_modeFields = Map.fromList [(modeFill1, odeFill1), (modeFill2, odeFill2)],
--            hybsys_modeInvariants = Map.fromList [(modeFill1, invariant), (modeFill2, invariant)],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList [(event1To2, (modeFill2, id)), (event2To1, (modeFill1, id))],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeFill1 = HybSysMode "fill1"
--    modeFill2 = HybSysMode "fill2"
--    odeFill1 :: [f] -> [f]
--    odeFill1 [_x1,_x2] = [newConstFnFromSample _x1 (w - v1), newConstFnFromSample _x1 (- v2)]
--    odeFill2 :: [f] -> [f]
--    odeFill2 [_x1,_x2] = [newConstFnFromSample _x1 (- v1), newConstFnFromSample _x1 (w - v2)]
--    invariant [x1,x2] = [makeNonneg x1, makeNonneg x2]
--    event1To2 = HybSysEventKind "1To2"
--    event2To1 = HybSysEventKind "2To1"
--    prune1To2 [x1,_x2] = [x1, 0]
--    prune2To1 [_x1,x2] = [0, x2]
--
--    eventSpecMap (HybSysMode "fill1") =
--        Map.singleton event1To2 $
--            ([True, True], x2Dip, const (Just True), prune1To2)
--        where
--        x2Dip [_x1,x2] = x2
--    eventSpecMap (HybSysMode "fill2") =
--        Map.singleton event2To1 $
--            ([True, True], x1Dip, const (Just True), prune2To1)
--        where
--        x1Dip [x1,_x2] = x1
--
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeFill1 initValues,
--            hybivp_maybeExactStateAtTEnd = Just $
--                Map.fromList
--                [
--                    (modeFill1, [0,0]),
--                    (modeFill2, [0,0])
--                ]
--        }
--    description =
--        ""
--        ++    "if fill1 then (if x2 = 0 then fill2 else x1' = 4-2, x2' =  -3)"
--        ++ "\n if fill2 then (if x1 = 0 then fill1 else x1' =  -2, x2' = 4-3)"
--        ++ "\n ; x1(" ++ show tStart ++ ") = " ++ show initX1
--        ++    ", x2(" ++ show tStart ++ ") = " ++ show initX2
--    initValues@[initX1, initX2] = [1,1] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp
--
--ivpTwoTanksSum_AfterZeno :: Domain f -> HybridIVP f
--ivpTwoTanksSum_AfterZeno tEndMinusTZeno =
--    ivp
--    where
--    v1 = 2 :: Domain f
--    v2 = 3 :: Domain f
--    w = 4 :: Domain f
--    tZeno = 2
--    tEnd = tEndMinusTZeno + tZeno
--    system =
--        HybridSystem
--        {
--            hybsys_componentNames = ["x1","x2","x12"],
--            hybsys_modeFields = Map.fromList 
--                [(modeFill1, odeFill1), 
--                 (modeFill2, odeFill2)
--                ],
--            hybsys_modeInvariants = Map.fromList 
--                [(modeFill1, invariant), 
--                 (modeFill2, invariant)
--                ],
--            hybsys_eventModeSwitchesAndResetFunctions =
--                Map.fromList 
--                    [(event1To2, (modeFill2, id)), 
--                     (event2To1, (modeFill1, id))
--                    ],
--            hybsys_eventSpecification = eventSpecMap
--        }
--    modeFill1 = HybSysMode "fill1"
--    modeFill2 = HybSysMode "fill2"
--    odeFill1 :: [f] -> [f]
--    odeFill1 [_x1,_x2,_x12] = 
--        [newConstFnFromSample _x1 (w - v1), 
--         newConstFnFromSample _x1 (- v2), 
--         newConstFnFromSample _x1 (w - v1- v2)
--        ]
--    odeFill2 :: [f] -> [f]
--    odeFill2 [_x1,_x2,_x12] = 
--        [newConstFnFromSample _x1 (- v1), 
--         newConstFnFromSample _x1 (w - v2),
--         newConstFnFromSample _x1 (w - v1- v2)
--        ]
----    invariant = id
--    invariant [x1,x2,x12] =
--        [x1NN <\/> (makeNonneg $ x12NN - x2NN),
--         x2NN <\/> (makeNonneg $ x12NN - x1NN), 
--         x12NN <\/> (x1NN + x2NN)
--        ]
--        where
--        x1NN = makeNonneg x1
--        x2NN = makeNonneg x2
--        x12NN = makeNonneg x12
--    event1To2 = HybSysEventKind "1To2"
--    event2To1 = HybSysEventKind "2To1"
--    prune1To2 [x1,_x2, x12] = [x1, 0, x12]
--    prune2To1 [_x1,x2, x12] = [0, x2, x12]
--
--    eventSpecMap (HybSysMode "fill1") =
--        Map.singleton event1To2 $
--            ([True, True, True], x2Dip, const (Just True), prune1To2)
--        where
--        x2Dip [_x1,x2,_x12] = x2
--    eventSpecMap (HybSysMode "fill2") =
--        Map.singleton event2To1 $
--            ([True, True, True], x1Dip, const (Just True), prune2To1)
--        where
--        x1Dip [x1,_x2,_x12] = x1
--
--
--    ivp :: HybridIVP f
--    ivp =
--        HybridIVP
--        {
--            hybivp_description = description,
--            hybivp_system = system,
--            hybivp_tVar = "t",
--            hybivp_tStart = 0,
--            hybivp_tEnd = tEnd,
--            hybivp_initialStateEnclosure = 
--                Map.singleton modeFill1 initValues,
--            hybivp_maybeExactStateAtTEnd =
--                Nothing 
----                Just $
----                HybridSystemUncertainState 
----                {
----                    hybstate_modes = Set.fromList [modeFlow],
----                    hybstate_values = [0, 0, 0]
----                }
--        }
--    description =
--        ""
--        ++    "if fill1 then (if x2 = 0 then fill2 else x1' = 4-2, x2' =  -3)"
--        ++ "\n if fill2 then (if x1 = 0 then fill1 else x1' =  -2, x2' = 4-3)"
--        ++ "\n invariant x12 = x1 + x2"
--        ++ "\n ; x1(" ++ show tStart ++ ") = " ++ show initX1
--        ++    ", x2(" ++ show tStart ++ ") = " ++ show initX2
--        ++    ", x12(" ++ show tStart ++ ") = " ++ show initX12
--    initValues@[initX1, initX2, initX12] = [1,1,2] :: [Domain f]
--    tStart = hybivp_tStart ivp
----    tEnd = hybivp_tEnd ivp
----    tVar = hybivp_tVar ivp


dblToDom ::
    (ArithInOut.RoundedReal dom)
    => 
    dom -> Double -> dom
dblToDom sampleDom n =
    z <+>| n
    where
    z = zero sampleDom
    
makeNonneg ::
    (HasZero d, NumOrd.PartialComparison d, RefOrd.IntervalLike d) 
    => 
    d -> d
makeNonneg r
    | rangeContainsZero =
        RefOrd.fromEndpointsOutWithDefaultEffort (z, rR)
    | otherwise = r 
    where
    rangeContainsZero =
        ((rL <=? z) == Just True)
        &&
        ((z <=? rR) == Just True)
    z = zero r
    (rL, rR) = RefOrd.getEndpointsOutWithDefaultEffort r
    
    