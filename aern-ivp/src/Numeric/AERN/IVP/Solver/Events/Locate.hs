{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Events.Locate
    Description :  hybrid system simulation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Hybrid system simulation.
-}

module Numeric.AERN.IVP.Solver.Events.Locate
--(
--)
where

import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
--import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

--import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
--import qualified Data.Set as Set
--import qualified Data.List as List

import Numeric.AERN.Misc.Debug
_ = unsafePrint

    
{-
    Do binary splitting to a certain depth and for each segment get one of the following three values:

    N: event nowhere on this segment
    S: event certainly on this segment
    M: no idea
    
    If a segment returns N, do not split it any further.  
    
    By splitting like this to a certain depth we get a finite sequence over the alphabet {N,S,M}.
    We then reduce the sequence to a normal form using the following rewrite rules:
    
    S_ -> S
    NN -> N
    MNM -> M
    MM -> M
    MNS -> S
    MS -> S

    The following are all the possible normal forms:
    S, NS, N, M, MN, NM, NMN
    
    At the same time, with each S and M, associate a segment within T.  Some of the above rules enlarge this segment.
    
    A quicker way to compute the normal form and the segment of first occurence: 
        ignore all N segments unless there are only N segments, in which case return N
        if there is some S, ignore everything after the first S and merge it with all preceding M's, returning S
        if there is no S, merge all M's, returning M
    
    
    typical simple call:
        locateFirstZeroDip stepLimit (const $ Just True) [dipFn] dipFn 
-}
locateFirstZeroDip :: 
    (RefOrd.IntervalLike (Domain f),
     ArithInOut.RoundedReal (Domain f),
     CanEvaluate f) 
    =>
    Domain f -> 
    ([Domain f] -> Maybe Bool) -> 
    [f] -> 
    f -> 
    Maybe (Domain f, Bool)
locateFirstZeroDip stepLimit otherResetCondition allFns dipFn = 
    case locateBySplitting $ RefOrd.getEndpointsOutWithDefaultEffort tDom of
        LDResNone -> Nothing
        LDResSure dom -> Just (dom, True)
        LDResMaybe dom -> Just (dom, False)
    where
    locateBySplitting d@(dLE, dRE) 
        | ((stepLimit <? size) /= Just True) = 
            resD -- no more splitting
        | otherwise =
            case resD of
                LDResNone -> resD -- no point splitting in this case
                -- TODO: stop splitting also when dipFn includes zero throughout 
                _ -> combineLocateDipResults resL resR
        where
        resD = examineDipOnDom otherResetCondition allFns dipFn d 
        size = dRE <-> dLE
        resL = locateBySplitting (dLE, dM)
        resR = locateBySplitting (dM, dRE)
        (dM, _) = RefOrd.getEndpointsOutWithDefaultEffort $ (dLE <+> dRE) </>| (2 :: Int) 
    [(_tVar,tDom)] = toAscList $ getDomainBox sampleFn
    (sampleFn:_) = allFns
    
    
examineDipOnDom :: 
    (RefOrd.IntervalLike (Domain f), 
     CanEvaluate f,
     NumOrd.PartialComparison (Domain f), 
     HasZero (Domain f)) 
    =>
    ([Domain f] -> Maybe Bool) -> 
    [f] -> 
    f -> 
    (Domain f, Domain f) -> 
    LocateDipResult (Domain f)
examineDipOnDom otherResetCondition allFns dipFn (dLE, dRE) =
    case (z <? dipFnOnD, dipFnOnDR <? z, otherResetCondition allFnsOnD) of
        (Just True, _, _) -> LDResNone -- no dip
        (_, _, Just False) -> LDResNone -- other condition definitely false
        (_, Just True, Just True) -> LDResSure d 
            -- dip must have occured within d because the fn is below 0 in the end 
            -- and the other condition also holds on the whole of d, 
            --   so it held also at the time of the dip
        _ -> LDResMaybe d
            -- in all other cases, we declare that we don't know for sure
            -- TODO: LDResMaybe d enclosesZero
    where
    dipFnOnD = evalAtPointOutEff effEval boxD dipFn
    allFnsOnD = map (evalAtPointOutEff effEval boxD) $ allFns
    boxD = fromList [(tVar, d)]
    d = RefOrd.fromEndpointsOutWithDefaultEffort (dLE, dRE)
    dipFnOnDR = evalAtPointOutEff effEval boxDR dipFn
--    allFnsOnDR = map (evalAtPointOutEff effEval boxDR) $ allFns
    boxDR = fromList [(tVar, dRE)]
    effEval = evaluationDefaultEffort sampleFn
    z = zero sampleDom
    [(tVar,_)] = toAscList $ getDomainBox sampleFn
    sampleFn = dipFn
    sampleDom = dLE
    
    
data LocateDipResult dom =
      LDResNone
    | LDResSure dom
    | LDResMaybe dom

combineLocateDipResults :: 
  (RefOrd.RoundedLattice dom) 
  =>
  LocateDipResult dom -> 
  LocateDipResult dom -> 
  LocateDipResult dom
combineLocateDipResults LDResNone res2 = res2
combineLocateDipResults res1@(LDResSure _) _ = res1
combineLocateDipResults (LDResMaybe dom1) (LDResMaybe dom2) =
    let ?joinmeetEffort = RefOrd.joinmeetDefaultEffort dom1 in  
    LDResMaybe $ dom1 </\> dom2
combineLocateDipResults (LDResMaybe dom1) (LDResSure dom2) = 
    let ?joinmeetEffort = RefOrd.joinmeetDefaultEffort dom1 in  
    LDResSure $ dom1 </\> dom2  
    