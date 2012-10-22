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
import Numeric.AERN.RealArithmetic.RefinementOrderRounding (dblToReal)
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
     RefOrd.RoundedLattice f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedDivide f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    String {-^ IVP name - see source code for the list -} -> 
    f {-^ sample function of the type to be used in simulation -} -> 
    Maybe (HybridIVP f)
ivpByName name sampleFn =
    Map.lookup name $ ivpByNameMap sampleFn
    
ivpByNameReportError ::
    (Var f ~ String,
     HasConstFns f,
     RefOrd.RoundedLattice f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedDivide f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    String -> 
    f -> 
    HybridIVP f
ivpByNameReportError ivpName samplePoly =
    case ivpByName ivpName samplePoly of
        Just ivp -> ivp
        _ -> 
            error $
                "unknown ivp: " ++ ivpName
                ++ "\n known ivps:\n"
                ++ unlines (map ("    " ++) (ivpNames samplePoly))
    
ivpNames :: 
    (Var f ~ String,
     HasConstFns f,
     RefOrd.RoundedLattice f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedDivide f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    =>
    f -> [String]
ivpNames sampleFn = Map.keys $ ivpByNameMap sampleFn
    
ivpByNameMap ::
    (Var f ~ String,
     HasConstFns f,
     RefOrd.RoundedLattice f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedDivide f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    =>
    f ->
    Map.Map String (HybridIVP f)    
ivpByNameMap sampleFn =
    Map.fromList
    [
        ("expDec-resetOnce", ivpExpDecay_resetTHalf sampleFn),
        ("expDec-resetOn34", ivpExpDecay_resetOn34 sampleFn),
        ("springMass-resetOnce", ivpSpringMass_resetTHalf sampleFn),
        ("springMass-resetOn34", ivpSpringMass_resetOn34 sampleFn),
        ("bouncingBall-after1", ivpBouncingBall_AfterBounce 1 sampleFn),
        ("bouncingBall-after2", ivpBouncingBall_AfterBounce 2 sampleFn),
        ("bouncingBall-after3", ivpBouncingBall_AfterBounce 3 sampleFn),
        ("bouncingBall-after4", ivpBouncingBall_AfterBounce 4 sampleFn),
        ("bouncingBall-after5", ivpBouncingBall_AfterBounce 5 sampleFn),
        ("bouncingBall-after6", ivpBouncingBall_AfterBounce 6 sampleFn),
        ("bouncingBall-after7", ivpBouncingBall_AfterBounce 7 sampleFn),
        ("bouncingBall-after8", ivpBouncingBall_AfterBounce 8 sampleFn),
        ("bouncingBall-after9", ivpBouncingBall_AfterBounce 9 sampleFn),
        ("bouncingBall-after10", ivpBouncingBall_AfterBounce 10 sampleFn),
        ("bouncingBall-after16", ivpBouncingBall_AfterBounce 16 sampleFn),
        ("bouncingBall-after20", ivpBouncingBall_AfterBounce 20 sampleFn),
        ("bouncingBall-after30", ivpBouncingBall_AfterBounce 30 sampleFn),
        ("bouncingBall-after40", ivpBouncingBall_AfterBounce 40 sampleFn),
        ("bouncingBall-zeno", ivpBouncingBall_AfterZeno 0  sampleFn),
        ("bouncingBall-zenoPlus1Over2", ivpBouncingBall_AfterZeno 0.5 sampleFn),
        ("bouncingBall-zenoPlus2", ivpBouncingBall_AfterZeno 2 sampleFn),
        ("bouncingBallEnergy-zeno", ivpBouncingBallEnergy_AfterZeno 0 sampleFn),
        ("bouncingBallEnergy-zenoPlus1Over2", ivpBouncingBallEnergy_AfterZeno 0.5 sampleFn),
        ("bouncingBallEnergy-zenoPlus2", ivpBouncingBallEnergy_AfterZeno 2 sampleFn),
        ("bouncingBallEnergyRise-zeno", ivpBouncingBallEnergyRise_AfterZeno 0 sampleFn),
        ("bouncingBallEnergyRise-zenoPlus1Over2", ivpBouncingBallEnergyRise_AfterZeno 0.5 sampleFn),
        ("bouncingBallEnergyRise-zenoPlus2", ivpBouncingBallEnergyRise_AfterZeno 2 sampleFn),
        ("bouncingBallVibr-graze", ivpBouncingBallVibr_AtTime 2 sampleFn),
    -- TODO: define "bouncingBallVibrEnergy-graze" 
        ("bouncingBallDrop", ivpBouncingBallDrop_AtTime 3 2 0 5 sampleFn),
        ("bouncingBallEnergyDrop", ivpBouncingBallEnergyDrop_AtTime 3 2 0 5 sampleFn),
        ("twoBouncingBallsDrop", ivpTwoBouncingBallsDrop_AtTime 30 20 25 10 45 sampleFn),
        ("twoBouncingBallsEnergyDrop", ivpTwoBouncingBallsEnergyDrop_AtTime 30 20 25 10 45 sampleFn),
    -- TODO: fix breakage at time 20
        ("bouncingSpring-4", ivpBouncingSpring_AtTime 4 sampleFn),
        ("twoTanks-zenoMinus1Over16", ivpTwoTanks_AfterZeno (-1/16) sampleFn),
        ("twoTanks-zeno", ivpTwoTanks_AfterZeno 0 sampleFn),
        ("twoTanks-zenoPlus1Over2", ivpTwoTanks_AfterZeno 0.5 sampleFn),
        ("twoTanks-zenoPlus2", ivpTwoTanks_AfterZeno 2 sampleFn),
        ("twoTanksSum-zenoMinus1Over16", ivpTwoTanksSum_AfterZeno (-1/16) sampleFn),
        ("twoTanksSum-zeno", ivpTwoTanksSum_AfterZeno 0 sampleFn),
        ("twoTanksSum-zenoPlus1Over2", ivpTwoTanksSum_AfterZeno 0.5 sampleFn)
    ]

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
    (Just tEndDbl) = ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort tEnd (0::Double)) 0 tEnd
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpExpDecay_resetOn34 ::
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f)
    )
    => 
    f -> HybridIVP f
ivpExpDecay_resetOn34 (sampleFn :: f) =
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
    odeNormal :: [f] -> [f]
    odeNormal [x] = [neg x]
    eventReset = HybSysEventKind "reset"
    resetReset :: [f] -> [f]
    resetReset [x] = [newConstFnFromSample x initValue]
    eventSpecMap _mode =
        Map.singleton eventReset $
            ([True], xDip, const (Just True), id)
        where
        xDip [x] = x <-> xEventFn
            where
            xEventFn = newConstFnFromSample x $ (toDom 1) <*>| xEventDbl

    xEventDbl = 0.75 :: Double
    
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
                Map.singleton modeNormal [initValue],
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeNormal [xEnd]
        }
    description =
        "v = -x; if x <= " ++ show xEventDbl ++ " then x := " ++ show initValue 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
    initValue = (toDom 1) :: Domain f
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp
    xEnd = (toDom 1) <*>| (exp (-tEndDbl-3*(log xEventDbl)) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort tEnd (0::Double)) 0 tEnd
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn


ivpSpringMass_resetTHalf ::
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f)
    )
    => 
    f -> HybridIVP f
ivpSpringMass_resetTHalf (sampleFn :: f) =
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
    odeBefore, odeAfter :: [f] -> [f]
    odeBefore [x,v,time] = [v, neg x, newConstFnFromSample time (toDom 1)]
    odeAfter = odeBefore
    eventReset = HybSysEventKind "reset"
    resetReset :: [f] -> [f]
    resetReset [x,_v,time] = map (newConstFnFromSample x) initValues ++ [time]
    eventSpecMap (HybSysMode "after") = Map.empty -- reset only once!
    eventSpecMap _ =
        Map.singleton eventReset $
            ([True,True,True], timeDip, const (Just True), timeReset)
        where
        timeDip [_, _, t] = tEventP <-> t
            where
            tEventP = newConstFnFromSample t $ (toDom 1) <*>| tEventDbl
        timeReset [x,v,t] = [x,v,zP]
            where
            zP = zero t
    tEventDbl = 0.5 :: Double
    tEvent = ((zero sampleDom) :: Domain f) <+>| tEventDbl
    
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
                Map.singleton modeBefore (initValues ++ [tStart]),
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeAfter [xEnd, xDerEnd, tEnd <-> tEvent]
        }
    description =
        "x'' = -x; if t = " ++ show tEventDbl ++ " then [x,v] := " ++ show initValues 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [toDom 1,toDom 0] :: [Domain f]
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    xEnd = (toDom 1) <*>| (cos (tEndDbl - tEventDbl) :: Double)
    xDerEnd = (toDom $ -1) <*>| (sin (tEndDbl - tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort tEnd (0::Double)) 0 tEnd
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn
    
    
ivpSpringMass_resetOn34 :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f)
    )
    => 
    f -> HybridIVP f
ivpSpringMass_resetOn34 (sampleFn :: f) =
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
    odeNormal :: [f] -> [f]
    odeNormal [x,v] = [v, neg x]
    eventReset = HybSysEventKind "reset"
    resetReset :: [f] -> [f]
    resetReset [x,_v] = map (newConstFnFromSample x) initValues
    eventSpecMap _mode =
        Map.singleton eventReset $
            ([True, True], xDip, const (Just True), id)
        where
        xDip [x,_v] = x <-> xEventFn
            where
            xEventFn = newConstFnFromSample x $ (toDom 1) <*>| xEventDbl
    xEventDbl = 0.75 :: Double
    tEventDbl = acos xEventDbl -- 0.72273424781341...
    
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
                Map.singleton modeNormal (initValues ++ [tStart]),
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeNormal [xEnd, xDerEnd, tEnd]
        }
    description =
        "x'' = -x; if x <= " ++ show xEventDbl ++ " then [x,v] := " ++ show initValues 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [one sampleDom,zero sampleDom] :: [Domain f]
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
--    tVar = hybivp_tVar ivp
    xEnd = (one sampleDom) <*>| (cos (tEndDbl - tEventDbl) :: Double)
    xDerEnd = (toDom $ -1) <*>| (sin (tEndDbl - tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort tEnd (0::Double)) 0 tEnd
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpBouncingBall_AfterBounce :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Int -> 
    f -> 
    HybridIVP f
ivpBouncingBall_AfterBounce n =
    ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 3*(1 - 2^^(-n))
    xEnd = 5 * (2^^(-2*n))
    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero

ivpBouncingBall_AfterZeno :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f -> 
    HybridIVP f
ivpBouncingBall_AfterZeno howLong =
    ivpBouncingBall_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 3 + howLong
    xEnd = 0
    xDerEnd = 0

ivpBouncingBall_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    [Double] -> 
    f -> 
    HybridIVP f
ivpBouncingBall_AtTime tEndDbl [xEndDbl, xDerEndDbl] (sampleFn :: f) =
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
    odeMove :: [f] -> [f]
    odeMove [x,v] = [v, newConstFnFromSample x (toDom $ -10)]
--    invariantMove = id
    invariantMove [x,v] = [makeNonneg x,v]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v] = [toDom 0, neg $ makeNonneg  $ neg v]
    resetBounce :: [f] -> [f]
    resetBounce [x,v] = 
        [x, (-0.5 :: Double) |<*> v]
--        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v] = x
        vNegative [_x,v] = (v <? z)
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = toDom 0,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Just $
                Map.singleton modeMove $ map toDom [xEndDbl, xDerEndDbl]
        }
    description =
        "if x = 0 && v <= 0 then post(v) = -0.5*pre(v) else x'' = -10" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [toDom 5, toDom 0] :: [Domain f]
    tStart = hybivp_tStart ivp
    z = toDom 0
    tEnd = toDom tEndDbl
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpBouncingBallEnergy_AfterBounce ::
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Int -> 
    f -> 
    HybridIVP f
ivpBouncingBallEnergy_AfterBounce n =
    ivpBouncingBallEnergy_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = (3*(1 - 2^^(-n)) :: Double)
    xEnd = (5 * (2^^(-2*n)) :: Double)
    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero

ivpBouncingBallEnergy_AfterZeno :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f -> 
    HybridIVP f
ivpBouncingBallEnergy_AfterZeno howLong =
    ivpBouncingBallEnergy_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 3 + howLong
    xEnd = 0
    xDerEnd = 0

ivpBouncingBallEnergy_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    [Double] -> 
    f -> 
    HybridIVP f
ivpBouncingBallEnergy_AtTime tEndDbl [xEndDbl, vEndDbl] (sampleFn :: f) =
    ivp
    where
    rEnd = energyWith xEnd vEnd
    energyWith x v = z </\> (v <*> v <+> (toDom 20) <*> x)
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
    odeMove :: [f] -> [f]
    odeMove [x,v,r] = [v, newConstFnFromSample x (toDom $ -10), newConstFnFromSample r (toDom 0)]
    invariantMove [x,v,r] =
        [xNN <\/> x2, 
         v <\/> ((neg absV) </\> absV), 
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
        absV = sqrtOut $ makeNonneg $ rNN <-> ((toDom 20) <*> xNN)
        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> (toDom 20)
    eventBounce = HybSysEventKind "bc"
    pruneBounce [_x,v,r] = [z, neg $ makeNonneg $ neg v,r]
    resetBounce :: [f] -> [f]
    resetBounce [x,v,r] = 
        [x, 
         (-0.5 :: Double) |<*> v, 
         (0.25 :: Double) |<*> r]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True, True], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v, _r] = x
        vNegative [_x,v,_r] = (v <? z)
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
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
    initValues@[initX, initX', initR] = [toDom 5, toDom 0, energyWith initX initX'] :: [Domain f]
    tStart = hybivp_tStart ivp
    [xEnd, vEnd] = map toDom [xEndDbl, vEndDbl]
    tEnd = toDom tEndDbl
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpBouncingBallEnergyRise_AfterBounce ::
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Int -> 
    f -> 
    HybridIVP f
ivpBouncingBallEnergyRise_AfterBounce n =
    ivpBouncingBallEnergyRise_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = (3*(1 - 2^^(-n)) :: Double)
    xEnd = (5 * (2^^(-2*n)) :: Double)
    xDerEnd = 0 -- exactly between two bounces, the ball brieflly stops, ie its speed is zero

ivpBouncingBallEnergyRise_AfterZeno :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f -> 
    HybridIVP f
ivpBouncingBallEnergyRise_AfterZeno howLong =
    ivpBouncingBallEnergyRise_AtTime tEnd [xEnd, xDerEnd]
    where
    tEnd = 3 + howLong
    xEnd = 0
    xDerEnd = 0



ivpBouncingBallEnergyRise_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    [Double] -> 
    f -> 
    HybridIVP f
ivpBouncingBallEnergyRise_AtTime tEndDbl [xEndDbl, vEndDbl] (sampleFn :: f) =
    ivp
    where
    rEnd = energyWith xEnd vEnd
    energyWith x v = z </\> (v <*> v <+> (toDom 20) <*> x)
        -- added zero so that after reset the interval refines the original (model-level hack!)  
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","v","r"],
            hybsys_modeFields = Map.fromList [(modeRise, odeMove), (modeFall, odeMove)],
            hybsys_modeInvariants = Map.fromList [(modeRise, invariantRise), (modeFall, invariantFall)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList 
                    [(eventBounce, (modeRise, resetBounce)), 
                     (eventPeak, (modeFall, resetPeak))],
            hybsys_eventSpecification = eventSpecMap
        }
    modeRise = HybSysMode "rise"
    modeFall = HybSysMode "fall"
    odeMove :: [f] -> [f]
    odeMove [x,v,r] = [v, newConstFnFromSample x (toDom $ -10), newConstFnFromSample r (toDom 0)]
    invariantRise [x,v,r] =
        [xNN <\/> x2, -- x >=0 && x = (r - v^2)/20
         vNN <\/> absV, -- v >=0 && |v| = sqrt(r - 20*x)
         rNN] -- r >= 0
        {- making use of the energy conservation law: 
           (v)^2 + 2gx = r
           
           which implies 
           |v| = sqrt(r - 2gx) 
           x = (r - (v)^2) / 2g 
        -}
        where
        rNN = makeNonneg r
        xNN = makeNonneg x
        vNN = makeNonneg v
        absV = sqrtOut $ makeNonneg $ rNN <-> ((toDom 20) <*> xNN)
        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> (toDom 20)
    invariantFall [x,v,r] =
        [xNN <\/> x2, -- x >=0 && x = (r - v^2)/20
         vNP <\/> (neg absV), -- v <= 0 && |v| = sqrt(r - 20*x)
         rNN] -- r >= 0
        {- making use of the energy conservation law: 
           (v)^2 + 2gx = r
           
           which implies 
           |v| = sqrt(r - 2gx) 
           x = (r - (v)^2) / 2g 
        -}
        where
        rNN = makeNonneg r
        xNN = makeNonneg x
        vNP = neg $ makeNonneg (neg v)
        absV = sqrtOut $ makeNonneg $ rNN <-> ((toDom 20) <*> xNN)
        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> (toDom 20)
    eventBounce = HybSysEventKind "bc"
    pruneBounce [_x,v,r] = [z, neg $ makeNonneg $ neg v,r]
    resetBounce :: [f] -> [f]
    resetBounce [x,v,r] = 
        [x, 
         (-0.5 :: Double) |<*> v, 
         (0.25 :: Double) |<*> r]
    eventPeak = HybSysEventKind "pk"
    prunePeak [x,v,r] = [x, z, r]
    resetPeak :: [f] -> [f]
    resetPeak [x,v,r] = [x,v,r] 
    eventSpecMap mode 
        | mode == modeFall =
            Map.singleton
                eventBounce ([True, True, True], xDip, const (Just True), pruneBounce)
        | mode == modeRise =
            Map.singleton
                eventPeak ([False, True, False], vDip, const (Just True), prunePeak)
        where
        xDip [x,_v, _r] = x
        vDip [_x,v, _r] = v
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeFall initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "" ++ "if x = 0 && v <= 0 then post(v) = -v/2, post(r) = r/4 else x''= -10, r' = 0, r = v^2+20x, x >= 0, r >= 0)" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
        ++ ", r(" ++ show tStart ++ ") ∊ " ++ show initR
    initValues@[initX, initX', initR] = [toDom 5, toDom 0, energyWith initX initX'] :: [Domain f]
    tStart = hybivp_tStart ivp
    [xEnd, vEnd] = map toDom [xEndDbl, vEndDbl]
    tEnd = toDom tEndDbl
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpBouncingBallVibr_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f ->
    HybridIVP f
ivpBouncingBallVibr_AtTime tEndDbl (sampleFn :: f) =
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
    odeMove :: [f] -> [f]
    odeMove [x,v,y,w] = 
        [v, newConstFnFromSample x (toDom $ -0.81056947), -- 8/pi^2 
         w, (1::Double) |<*> (neg y)]
--    invariantMove = id
    invariantMove [x,v,y,w] = [y <+> (makeNonneg (x <-> y)),v,y,w]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v,y,w] = [y, w <-> (makeNonneg (w <-> v)),y,w]
    resetBounce :: [f] -> [f]
    resetBounce [x,v,y,w] = 
        [x, w <+> ((-0.5 :: Double) |<*> (v <-> w)), y, w]
--        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True, False, False], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v,y,_w] = x <-> y
        vNegative [_x,v,_y,w] = (v <-> w <? z)

    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
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
    initValues@[initX, initV, initY, initW] = (map toDom [0,1.2732395,0,1]) :: [Domain f]
--    initValues@[initX, initX'] = [0,0] :: [Domain f]
    tStart = hybivp_tStart ivp
    tEnd = toDom tEndDbl
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn


ivpBouncingBallDrop_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    Double -> 
    Double -> 
    Double -> 
    f ->
    HybridIVP f
ivpBouncingBallDrop_AtTime groundInitDbl tDropDbl groundDropDbl tEndDbl (sampleFn :: f) =
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
    odeMove :: [f] -> [f]
    odeMove [x,v,_y,_tt] = 
        [v, newConstFnFromSample x (toDom $ -10), 
         newConstFnFromSample x (toDom 0), 
         newConstFnFromSample x (toDom 1)]
--    invariantMove = id
    invariantMove [x,v,y,tt] = [y <+> (makeNonneg (x<->y)),v,y,tt]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v,y,tt] = [y, neg (makeNonneg (neg v)),y,tt]
    resetBounce :: [f] -> [f]
    resetBounce [x,v,y,tt] = 
        [x, ((-0.5 :: Double) |<*> v), y, tt]
    eventDrop = HybSysEventKind "drop"
    pruneDrop [x,v,y,_tt] = [x,v,y,tDrop]
    resetDrop :: [f] -> [f]
    resetDrop [x,v,y,tt] = 
        [x, v, newConstFnFromSample y groundDrop, tt <+>| (1 :: Double)] -- jump tt to ensure the event is isolated (hack!!)
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
            tNearDrop [_x, _v, _y, tt] = tt <? (tDrop <+> (toDom 0.5))
        eventsBounce =
            Map.singleton eventBounce $
                ([True, True, False, False], xDip, vNegative, pruneBounce)
            where
            xDip [x,_v,y,_tt] = x <-> y
            vNegative [_x,v,_y,_tt] = (v <? z)
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = toDom 0,
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
    initValues@[initX, initV, initY, _initTT] = (map toDom [5,0,groundInitDbl,0]) :: [Domain f]
--    initValues@[initX, initX'] = [0,0] :: [Domain f]
    tStart = hybivp_tStart ivp
    [_groundInit, tDrop, groundDrop, tEnd] = map toDom [groundInitDbl, tDropDbl, groundDropDbl, tEndDbl]
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpBouncingBallEnergyDrop_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     RefOrd.RoundedLattice f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedDivide f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    Double -> 
    Double -> 
    Double -> 
    f ->
    HybridIVP f
ivpBouncingBallEnergyDrop_AtTime groundInitDbl tDropDbl groundDropDbl tEndDbl (sampleFn :: f) =
    ivp
    where
    energyWith x v = z </\> (v <*> v <+> (toDom 20) <*> x)
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
    odeMove :: [f] -> [f]
    odeMove [x,v,r,y,tt] = 
        [v, 
         newConstFnFromSample x (toDom $ -10), 
         newConstFnFromSample r (toDom 0), 
         newConstFnFromSample y (toDom 0), 
         newConstFnFromSample tt (toDom 1)]
--    invariantMove = id
    invariantMove [x,v,r,y,tt] = 
        [xNN <\/> x2, 
         v <\/> ((neg absV) </\> absV), 
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
        xNN = y <+> (makeNonneg (x <-> y))
        absV = sqrtOut $ makeNonneg $ rNN <-> ((toDom 20) <*> xNN)
        x2 = (rNN <-> (makeNonneg $ v <*> v)) </> (toDom 20)
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v,r,y,tt] = [y, neg (makeNonneg (neg v)),r,y,tt]
    resetBounce :: [f] -> [f]
    resetBounce [x,v,r,y,tt] = 
        [x, 
         ((-0.5 :: Double) |<*> v),
         y2g <+> ((r <-> y2g) </>| (4 :: Double)), -- Kinetic energy is scaled by 1/4  
         y, tt]
         where
         y2g = (20 :: Double) |<*> y
    eventDrop = HybSysEventKind "drop"
    pruneDrop [x,v,r,y,_tt] = [x,v,r,y,tDrop]
    resetDrop :: [f] -> [f]
    resetDrop [x,v,r,y,tt] = 
        [x, v, 
         zP </\> r, -- include 0 to create a refinement fixed point (hack!!)  
         newConstFnFromSample y groundDrop, 
         tt <+>| (1 :: Double)] -- move clock to avoid another drop event (hack!!)
        where
        zP = newConstFnFromSample r z
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
            tNearDrop [_x, _v,_r, _y, tt] = tt <? (tDrop <+> (toDom 0.5))
        eventsBounce =
            Map.singleton eventBounce $
                ([True, True, True, False, False], xDip, vNegative, pruneBounce)
            where
            xDip [x,_v,_r,y,_tt] = x <-> y
            vNegative [_x,v,_r,_y,_tt] = (v <? z)
   
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
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
    initValues@[initX, initV, initR, initY, _initTT] = 
        [toDom 5,toDom 0,
         energyWith initX initV,
         groundInit,
         toDom 0] :: [Domain f]
--    initValues@[initX, initX'] = [0,0] :: [Domain f]
    tStart = hybivp_tStart ivp
    [groundInit, tDrop, groundDrop, tEnd] = map toDom [groundInitDbl, tDropDbl, groundDropDbl, tEndDbl]
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpTwoBouncingBallsDrop_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    Double -> 
    Double -> 
    Double -> 
    Double -> 
    f ->
    HybridIVP f
ivpTwoBouncingBallsDrop_AtTime 
        groundInitDbl tDrop1Dbl tDrop2PreDbl groundDropDbl tEndDbl (sampleFn :: f) =
    ivp
    where
    g = 9.81 :: Double
    c = 0.8 :: Double
    tDrop2Dbl = tDrop2PreDbl + 1
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
    odeMove :: [f] -> [f]
    odeMove [x1,v1,y1,x2,v2,y2,tt] = 
        [v1, 
         newConstFnFromSample x1 (toDom (-g)), 
         newConstFnFromSample y1 (toDom 0),
         v2, 
         newConstFnFromSample x2 (toDom (-g)), 
         newConstFnFromSample y2 (toDom 0),
         newConstFnFromSample tt (toDom 1)]
--    invariantMove = id
    invariantMove [x1,v1,y1,x2,v2,y2,tt] = 
        [y1 <+> (makeNonneg (x1 <-> y1)),v1,y1,
         y2 <+> (makeNonneg (x2 <-> y2)),v2,y2,
         tt]
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
    resetBounce1 :: [f] -> [f]
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
    resetDrop1 :: [f] -> [f]
    resetDrop1 [x1,v1,y1,x2,v2,y2,tt] = 
        [x1, v1, newConstFnFromSample y1 groundDrop,
         x2,v2,y2, 
         tt <+>| (1 :: Double)] -- jump tt to avoid another drop event (hack!!)
    resetDrop2 [x1,v1,y1,x2,v2,y2,tt] = 
        [x1,v1,y1,
         x2, v2, newConstFnFromSample y2 groundDrop, 
         tt <+>| (1 :: Double)] -- jump tt to avoid another drop event (hack!!)
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
            tNearDrop1 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tt <? (tDrop1 <+> (toDom 0.5))
        eventsDrop2 =
            Map.singleton eventDrop2
                ([False, False, False, False, False, True, True], tDip2, tNearDrop2, pruneDrop2)
            where
            tDip2 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tDrop2P <-> tt
                where
                tDrop2P = newConstFnFromSample tt tDrop2
            tNearDrop2 [_x1,_v1,_y1,_x2,_v2,_y2,tt] = tt <? (tDrop2 <+> (toDom 0.5))
        eventsBounce1 =
            Map.singleton eventBounce1 $
                ([True, True, False, False, False, False, False], 
                    x1Dip, v1Negative, pruneBounce1)
            where
            x1Dip [x1,_v1,y1,_x2,_v2,_y2,_tt] = x1 <-> y1
            v1Negative [_x1,v1,_y1,_x2,_v2,_y2,_tt] = (v1 <? z)
        eventsBounce2 =
            Map.singleton eventBounce2 $
                ([False, False, False, True, True, False, False], 
                    x2Dip, v2Negative, pruneBounce2)
            where
            x2Dip [_x1,_v1,_y1,x2,_v2,y2,_tt] = x2 <-> y2
            v2Negative [_x1,_v1,_y1,_x2,v2,_y2,_tt] = (v2 <? z)
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
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
    initValues@[initX1, initV1, initY1, initX2, initV2, initY2, _initTT] = 
        [toDom 30,toDom 14,groundInit,
         toDom 30,toDom 25,groundInit,
         z] :: [Domain f]
    tStart = hybivp_tStart ivp
    [groundInit, tDrop1, tDrop2, groundDrop, tEnd] = map toDom [groundInitDbl, tDrop1Dbl, tDrop2Dbl, groundDropDbl, tEndDbl]
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn


ivpTwoBouncingBallsEnergyDrop_AtTime ::
    (Var f ~ String,
     HasConstFns f,
     RefOrd.RoundedLattice f,
     Neg f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedMixedDivide f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedSquareRoot (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    Double -> 
    Double -> 
    Double -> 
    Double -> 
    f ->
    HybridIVP f
ivpTwoBouncingBallsEnergyDrop_AtTime
        groundInitDbl tDrop1Dbl tDrop2PreDbl groundDropDbl tEndDbl (sampleFn :: f) =
    ivp
    where
    tDrop2Dbl = tDrop2PreDbl + 1
    g = 9.81 :: Double
    c = 0.8 :: Double
    energyWith x v = z </\> (v <*> v <+> (toDom 20) <*> x)
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
    odeMove :: [f] -> [f]
    odeMove [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [v1,
         newConstFnFromSample x1 (toDom (-g)), 
         newConstFnFromSample r1 (toDom 0), 
         newConstFnFromSample y1 (toDom 0), 
         v2,
         newConstFnFromSample x2 (toDom (-g)), 
         newConstFnFromSample r2 (toDom 0), 
         newConstFnFromSample y2 (toDom 0),
         newConstFnFromSample tt (toDom 1)]
--    invariantMove = id
    invariantMove [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1NN <\/> x1E, 
         v1 <\/> ((neg absV1) </\> absV1), 
         r1NN,
         y1, 
         x2NN <\/> x2E, 
         v2 <\/> ((neg absV2) </\> absV2), 
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
        x1NN = y1 <+> (makeNonneg (x1 <-> y1))
        absV1 = sqrtOut $ makeNonneg $ r1NN <-> ((2*g) |<*> x1NN)
        x1E = (r1NN <-> (makeNonneg $ v1 <*> v1)) </>| (2*g)
        r2NN = makeNonneg r2
        x2NN = y2 <+> (makeNonneg (x2 <-> y2))
        absV2 = sqrtOut $ makeNonneg $ r2NN <-> ((2*g) |<*> x2NN)
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
    resetBounce1 :: [f] -> [f]
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
    resetDrop1 :: [f] -> [f]
    resetDrop1 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1, v1, 
         zP </\> r1, -- include 0 to create a refinement fixed point (hack!!)  
         newConstFnFromSample y1 groundDrop,
         x2,v2,r2,y2, 
         tt <+>| (1 :: Double)] -- move clock to avoid another drop event (hack!!)
        where
        zP = newConstFnFromSample tt z
    resetDrop2 [x1,v1,r1,y1,x2,v2,r2,y2,tt] = 
        [x1,v1,r1,y1, 
         x2, v2, 
         zP </\> r2, -- include 0 to create a refinement fixed point (hack!!)  
         newConstFnFromSample y2 groundDrop,
         tt <+>| (1 :: Double)] -- move clock to avoid another drop event (hack!!)
        where
        zP = newConstFnFromSample tt z
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
            tNearDrop1 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tt <? (tDrop1 <+> (toDom 0.5))
        eventsDrop2 =
            Map.singleton eventDrop2
                ([False, False, False, False, False, False, True, True, True], tDip2, tNearDrop2, pruneDrop2)
            where
            tDip2 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tDrop2P <-> tt
                where
                tDrop2P = newConstFnFromSample tt tDrop2
            tNearDrop2 [_x1,_v1,_r1,_y1,_x2,_v2,_r2,_y2,tt] = tt <? (tDrop2 <+> (toDom 0.5))
        eventsBounce1 =
            Map.singleton eventBounce1 $
                ([True, True, True, False, False, False, False, False, False], 
                    x1Dip, v1Negative, pruneBounce1)
            where
            x1Dip [x1,_v1,_r1,y1,_x2,_v2,_r2,_y2,_tt] = x1 <-> y1
            v1Negative [_x1,v1,_r1,_y1,_x2,_v2,_r2,_y2,_tt] = (v1 <? z)
        eventsBounce2 =
            Map.singleton eventBounce2 $
                ([False, False, False, False, True, True, True, False, False], 
                    x2Dip, v2Negative, pruneBounce2)
            where
            x2Dip [_x1,_v1,_r1,_y1,x2,_v2,_r2,y2,_tt] = x2 <-> y2
            v2Negative [_x1,_v1,_r1,_y1,_x2,v2,_r2,_y2,_tt] = (v2 <? z)
    
    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
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
        [toDom 30,toDom 14,energyWith initX1 initV1,groundInit,
         toDom 30,toDom 25,energyWith initX2 initV2,groundInit,
         z] :: [Domain f]
--    initValues@[initX, initX'] = [0,0] :: [Domain f]
    tStart = hybivp_tStart ivp
    [groundInit, tDrop1, tDrop2, groundDrop, tEnd] = 
        map toDom [groundInitDbl, tDrop1Dbl, tDrop2Dbl, groundDropDbl, tEndDbl]
    z = toDom 0
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpBouncingSpring_AtTime :: 
    (Var f ~ String,
     HasConstFns f,
     ArithInOut.RoundedMixedAdd f Double,
     ArithInOut.RoundedMixedMultiply f Double,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f -> 
    HybridIVP f
ivpBouncingSpring_AtTime tEndDbl (sampleFn :: f) =
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
    odeMove :: [f] -> [f]
    odeMove [x,v] = [v, (-1 :: Double) |<*> x]
    invariantMove [x,v] = [(toDom 1) <+> (makeNonneg (x <-> (toDom 1))), v]
    eventBounce = HybSysEventKind "bounce"
    pruneBounce [_x,v] = [toDom 1, neg $ makeNonneg $ neg v]
    resetBounce :: [f] -> [f]
    resetBounce [x,v] = 
        [x, (-0.5 :: Double) |<*> v]
--        [newConstFnFromSample v 0, (0 :: Double) |<*> v]
    eventSpecMap _mode =
        Map.singleton eventBounce $
            ([True, True], xDip, vNegative, pruneBounce)
        where
        xDip [x,_v] = x <+>| (-1 :: Double)
        vNegative [_x,v] = (v <? z)

    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeMove initValues,
            hybivp_maybeExactStateAtTEnd = Nothing
        }
    description =
        "if x = 1 && v <= 0 then post(v) = -0.5*pre(v) else x'' = -10x" 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initX
        ++ ", v(" ++ show tStart ++ ") = " ++ show initX'
    initValues@[initX, initX'] = [toDom 1,toDom 1] :: [Domain f]
    tStart = hybivp_tStart ivp
    z = toDom 0
    tEnd = toDom tEndDbl
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpTwoTanks_AfterZeno :: 
    (Var f ~ String,
     HasConstFns f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f -> 
    HybridIVP f
ivpTwoTanks_AfterZeno tEndMinusTZenoDbl (sampleFn :: f) =
    ivp
    where
    v1 = toDom 2
    v2 = toDom 3
    w = toDom 4
    tZenoDbl = 2
    tEndDbl = tEndMinusTZenoDbl + tZenoDbl
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
    odeFill1 :: [f] -> [f]
    odeFill1 [_x1,_x2] = [newConstFnFromSample _x1 (w <-> v1), newConstFnFromSample _x1 (neg v2)]
    odeFill2 :: [f] -> [f]
    odeFill2 [_x1,_x2] = [newConstFnFromSample _x1 (neg v1), newConstFnFromSample _x1 (w <-> v2)]
    invariant [x1,x2] = [makeNonneg x1, makeNonneg x2]
    event1To2 = HybSysEventKind "1To2"
    event2To1 = HybSysEventKind "2To1"
    prune1To2 [x1,_x2] = [x1, toDom 0]
    prune2To1 [_x1,x2] = [toDom 0, x2]

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

    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
            hybivp_tEnd = tEnd,
            hybivp_initialStateEnclosure = 
                Map.singleton modeFill1 initValues,
            hybivp_maybeExactStateAtTEnd = Just $
                Map.fromList
                [
                    (modeFill1, [toDom 0,toDom 0]),
                    (modeFill2, [toDom 0,toDom 0])
                ]
        }
    description =
        ""
        ++    "if fill1 then (if x2 = 0 then fill2 else x1' = 4-2, x2' =  -3)"
        ++ "\n if fill2 then (if x1 = 0 then fill1 else x1' =  -2, x2' = 4-3)"
        ++ "\n ; x1(" ++ show tStart ++ ") = " ++ show initX1
        ++    ", x2(" ++ show tStart ++ ") = " ++ show initX2
    initValues@[initX1, initX2] = [toDom 1,toDom 1] :: [Domain f]
    tStart = hybivp_tStart ivp
    z = toDom 0
    tEnd = toDom tEndDbl
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn

ivpTwoTanksSum_AfterZeno :: 
    (Var f ~ String,
     HasConstFns f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f)
    )
    => 
    Double -> 
    f -> 
    HybridIVP f
ivpTwoTanksSum_AfterZeno tEndMinusTZenoDbl (sampleFn :: f) =
    ivp
    where
    v1 = toDom 2
    v2 = toDom 3
    w = toDom 4
    tZenoDbl = 2
    tEndDbl = tEndMinusTZenoDbl + tZenoDbl
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
    odeFill1 :: [f] -> [f]
    odeFill1 [_x1,_x2,_x12] = 
        [newConstFnFromSample _x1 (w <-> v1), 
         newConstFnFromSample _x1 (neg v2), 
         newConstFnFromSample _x1 (w <-> v1 <-> v2)
        ]
    odeFill2 :: [f] -> [f]
    odeFill2 [_x1,_x2,_x12] = 
        [newConstFnFromSample _x1 (neg v1), 
         newConstFnFromSample _x1 (w <-> v2),
         newConstFnFromSample _x1 (w <-> v1 <-> v2)
        ]
--    invariant = id
    invariant [x1,x2,x12] =
        [x1NN <\/> (makeNonneg $ x12NN <-> x2NN),
         x2NN <\/> (makeNonneg $ x12NN <-> x1NN), 
         x12NN <\/> (x1NN <+> x2NN)
        ]
        where
        x1NN = makeNonneg x1
        x2NN = makeNonneg x2
        x12NN = makeNonneg x12
    event1To2 = HybSysEventKind "1To2"
    event2To1 = HybSysEventKind "2To1"
    prune1To2 [x1,_x2, x12] = [x1, toDom 0, x12]
    prune2To1 [_x1,x2, x12] = [toDom 0, x2, x12]

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


    ivp :: HybridIVP f
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = z,
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
    initValues@[initX1, initX2, initX12] = [toDom 1,toDom 1,toDom 2] :: [Domain f]
    tStart = hybivp_tStart ivp
    z = toDom 0
    tEnd = toDom tEndDbl
    toDom = dblToReal sampleDom
    sampleDom = getSampleDomValue sampleFn


makeNonneg ::
    (HasZero d, NumOrd.PartialComparison d, RefOrd.IntervalLike d) 
    => 
    d -> d
makeNonneg r
    | rangeContainsZero =
        RefOrd.fromEndpointsOutWithDefaultEffort (z, rR)
    | alreadyNonneg = r
    | otherwise = z
    where
    alreadyNonneg =
        (z <=? rL) == Just True
    rangeContainsZero =
        ((rL <=? z) == Just True)
        &&
        ((z <=? rR) == Just True)
    z = zero r
    (rL, rR) = RefOrd.getEndpointsOutWithDefaultEffort r
    
    