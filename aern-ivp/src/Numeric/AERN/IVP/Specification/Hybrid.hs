{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Specification.Hybrid
    Description :  specification of hybrid system IVP with uncertainty
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Specification of hybrid system IVP with uncertainty.
-}

module Numeric.AERN.IVP.Specification.Hybrid
--(
--)
where

import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.New

import qualified Data.Set as Set
import qualified Data.Map as Map

--import Numeric.AERN.RmToRn.Evaluation
--import Numeric.AERN.RmToRn.Integration
--
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
--
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort
--
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

--import Numeric.AERN.Misc.Debug



data HybridIVP f =
    HybridIVP
    {
        hybivp_description :: String
    ,
        hybivp_system :: HybridSystem f
    , 
        hybivp_tVar :: Var f -- ^ @tVar@
    ,
        hybivp_tStart :: Domain f -- ^ @tStart@
    ,
        hybivp_tEnd :: Domain f -- ^ @tEnd@
    ,
        hybivp_initialStateEnclosure :: HybridSystemUncertainState (Domain f) 
        {-^
        -}
    ,
        hybivp_maybeExactStateAtTEnd :: Maybe (HybridSystemUncertainState (Domain f))
    }

data HybridSystem f =
    HybridSystem
    {
        hybsys_componentNames :: [Var f]
    ,
        hybsys_modeFields :: Map.Map HybSysMode ([f] -> [f])
    ,
        hybsys_modeInvariants :: Map.Map HybSysMode ([Domain f] -> [Domain f])
    ,
        hybsys_eventModeSwitchesAndResetFunctions :: Map.Map HybSysEventKind (HybSysMode, [f] -> [f])
    ,
        hybsys_eventSpecification :: 
            HybSysMode ->
            Map.Map HybSysEventKind
                (
                 [Bool],
                    {- indication of which components are affected by the event;
                       a component is affected if the reset function changes its value OR
                       if the new mode has a different dynamics for this variable -}
                 [f] -> f,
                    {- construction of a zero crossing function -}
                 [Domain f] -> Maybe Bool,
                    {- additional condition -}
                 [Domain f] -> [Domain f] 
                    {- intersection with the support of the guard, 
                       used to prune some impossible states at the switching point -} 
                )  
    }
    
newtype HybSysMode = HybSysMode String deriving (Eq, Ord, Show)
newtype HybSysEventKind = HybSysEventKind String deriving (Eq, Ord, Show)

type HybridSystemUncertainState dom =
    Map.Map HybSysMode [dom]

--data HybridSystemUncertainStateOneMode f =
--    HybridSystemUncertainStateOneMode
--    {
--        hybstate_mode :: HybSysMode
--    ,
--        hybstate_values :: [Domain f]
--    }

mergeHybridStates ::
    (RefOrd.RoundedLattice dom)
    =>
    RefOrd.JoinMeetEffortIndicator dom ->
    HybridSystemUncertainState dom ->
    HybridSystemUncertainState dom ->
    HybridSystemUncertainState dom
mergeHybridStates effJoin state1 state2 =    
    Map.unionWith mergeValues state1 state2
    where
    mergeValues values1 values2 =
        let ?joinmeetEffort = effJoin in
        zipWith (</\>) values1 values2

getHybridStateUnion ::
    (RefOrd.RoundedLattice dom)
    =>
    RefOrd.JoinMeetEffortIndicator dom ->
    HybridSystemUncertainState dom ->
    (Set.Set HybSysMode, [dom])
getHybridStateUnion effJoin state =
    (Set.fromAscList modes, foldl1 mergeValues valueVecs) 
    where
    (modes, valueVecs) =
        unzip $ Map.toAscList state
    mergeValues values1 values2 =
        let ?joinmeetEffort = effJoin in
        zipWith (</\>) values1 values2
    
measureImprovementState :: 
      (HasImprecision dom,
       Imprecision dom ~ dom,
       ArithInOut.RoundedReal dom) 
      =>
      dom
      -> ArithInOut.RoundedRealEffortIndicator dom
      -> HybridSystemUncertainState dom
      -> HybridSystemUncertainState dom
      -> Imprecision dom
measureImprovementState sampleDom effDom state1 state2
    | newModeInState2 = neg (one sampleDom) -- =-1 serious deterioration
    | otherwise = 
        let ?mixedAddInOutEffort = effAddDomInt in
        modeCountDecreaseInState2 |<+> improvement 
    where
    newModeInState2 = 
        not $ Map.null $ Map.difference state2 state1
    modeCountDecreaseInState2 = 
        Map.size $ Map.difference state1 state2
    improvement =
        let ?addInOutEffort = effAddDom in
        Map.fold (<+>) (zero sampleDom) improvementsPerModeMap
    improvementsPerModeMap =
        Map.intersectionWith measureImprovementVec state1 state2

    measureImprovementVec vec1 vec2 = 
        let ?addInOutEffort = effAddDom in
        foldl1 (<+>) improvements
        where
        improvements = map measureImprovement $ zip vec1 vec2
    measureImprovement (encl1, encl2) =
        let ?addInOutEffort = effAddDom in
        (imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)

    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effAddDomInt = 
        ArithInOut.mxfldEffortAdd sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
    
    
    