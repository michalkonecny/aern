{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Picard
    Description :  IVP solvers using Picard operators
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Integrators of function enclosures.
-}

module Numeric.AERN.RmToRn.Picard 
(
    solveUncertainValueExactTime
)
where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug

solveUncertainValueExactTime ::
    (CanAddVariables f,
     CanEvaluate f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     Neg (Domain f), 
     RefOrd.RoundedLattice(Domain f), 
     RefOrd.IntervalLike(Domain f), 
     fvec ~ [f],
     Show f, Show (Domain f))
    =>
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    RefOrd.JoinMeetEffortIndicator (Domain f) ->
    Var f {-^ @t@ - the time variable -} ->
    Domain f {-^ @T@ - bounded time interval of interest -} ->
    fvec {-^ @a@ - functions giving the initial value parametrised by domain @D@ -}  ->
    (fvec -> fvec) {-^ the approximate vector field, transforming vectors of functions, all functions have the same domain -} ->
    Domain f {-^ initial widening @delta@ -}  ->
    [fvec] {-^ sequence of enclosures with domain @T x D@ produced by the Picard operator -}
solveUncertainValueExactTime
        effInteg effInclFn effAddFn effAddFnDom effJoinDom
        tVar timeDomain (initialValuesFns :: [f]) field delta
    =
    iterate picard firstEnclosure
    where
    firstEnclosure =
        findEnclosure (10 :: Int) $ iterate picard initialAttemptFns
    initialValuesFnsWithT =
        map (addVariablesFront [(tVar, timeDomain)]) initialValuesFns 
    initialAttemptFns =
        map widenFn initialValuesFnsWithT
        where
        widenFn fn =
            let ?mixedAddInOutEffort = effAddFnDom in
            fn <+>| wideningInterval 
        wideningInterval =
            let ?joinmeetEffort = effJoinDom in
            (neg delta) </\> delta
    findEnclosure maxIter (fn1Vec : fn2Vec : rest)
        | maxIter > 0 =
            case fn2RefinesFn1 of
                True -> fn2Vec
                _ ->
--                    unsafePrint
--                    (
--                        "solveUncertainValueExactTime: findEnclosure: not yet enclosing:"
--                        ++ "\n fn1Vec = " ++ (show $ evalAtEndTimeVec fn1Vec)
--                        ++ "\n fn2Vec = " ++ (show $ evalAtEndTimeVec fn2Vec)
--                        ++ "\n fn1Vec at end time = " ++ (show $ evalAtEndTimeVec fn1Vec)
--                        ++ "\n fn2Vec at end time = " ++ (show $ evalAtEndTimeVec fn2Vec)
--                    ) $
                    findEnclosure (maxIter - 1) (fn2Vec : rest)
        where
        fn2RefinesFn1 =
            let ?pCompareEffort = effInclFn in
            null $ filter (/= (Just True)) $ zipWith (|<=?) fn1Vec fn2Vec
    findEnclosure _ _  =
        error "aern-picard: solveUncertainValueExactTime failed to find enclosure"
    picard xvec = 
--        unsafePrint
--        (
--            "solveUncertainValueExactTime: picard:"
--            ++ "\n xvec = " ++ (show xvec)
--            ++ "\n xdvec = " ++ (show xdvec)
--            ++ "\n result = " ++ (show result)
--            ++ "\n xvec at end time = " ++ (show $ evalAtEndTimeVec xvec)
--            ++ "\n xdvec at end time = " ++ (show $ evalAtEndTimeVec xdvec)
--            ++ "\n result at end time = " ++ (show $ evalAtEndTimeVec result)
--        ) $
        result
        where
        result =
            let ?addInOutEffort = effAddFn in 
            zipWith (<+>) primitFn initialValuesFnsWithT
        primitFn = map picardFn xdvec
        xdvec = field xvec
        picardFn xdi =
            primitiveFunctionOutEff effInteg xdi tVar
    evalAtEndTimeVec fnVec =
        map evalAtEndTimeFn fnVec
    evalAtEndTimeFn :: f -> Domain f
    evalAtEndTimeFn fn =
        evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
        where
        endTimeArea :: DomainBox f
        endTimeArea = insertVar tVar timeDomainR $ getDomainBox fn
        (_, timeDomainR) = RefOrd.getEndpointsOutWithDefaultEffort timeDomain 
        
    