{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Laws
    Description :  auxiliary functions for testable properties  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Auxiliary functions for testable properties.
-}

module Numeric.AERN.RmToRn.Laws where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd

import Numeric.AERN.Basics.Arbitrary

import Numeric.AERN.Misc.QuickCheck
import Numeric.AERN.Misc.Debug
import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

roundedAgreeWithPointwisePred2 :: 
    (CanEvaluate f,
     Show f,
     Show (DomainBox f)) 
    =>
    f -> 
    String -> 
    (effFn -> f -> f -> Maybe Bool) -> 
    (effPt -> Domain f -> Domain f -> Maybe Bool) -> 
    (f, f) -> 
    (SingletonInArea (DomainBox f)) -> 
    effFn ->
    effPt ->
    EvaluationEffortIndicator f ->
    Bool
roundedAgreeWithPointwisePred2 _ contextDescription fnOpEff ptOpEff (fn1, fn2) (SingletonInArea box) effFnOp effPtOp effEval =
    case (resultFn, resultPt) of
        (Just True, Just False) -> -- if it is true on all points, it cannot be false on this point
            unsafePrint
            (
                contextDescription ++ ": pointwise check failed for: " 
                ++ "\n fn1 = " ++ show fn1
                ++ "\n fn2 = " ++ show fn2
                ++ "\n box = " ++ show box
            )
            False
        _ -> 
--            unsafePrint
--            (
--                contextDescription ++ ": pointwise check succeeded for: " 
--                ++ "\n box = " ++ show box
--                ++ "\n resultFn = " ++ show resultFn
--                ++ "\n resultPt = " ++ show resultPt
--            )
            True
    where
    resultFn = fnOpEff effFnOp fn1 fn2
    resultPt = ptOpEff effPtOp pt1 pt2
    pt1 = evalAtPointOutEff effEval box fn1
    pt2 = evalAtPointOutEff effEval box fn2

roundedRefinementIsotoneDom ::
    (RefOrd.PartialComparison t, 
     RefOrd.ArbitraryOrderedTuple (DomainBox f),
     Show (DomainBox f),
     Show (Domain f),
     Show t)
    =>
    f ->
    String ->
    (ei -> (DomainBox f) -> t) ->
    (ei -> (DomainBox f) -> t) ->
    (RefOrd.LEPair (DomainBox f)) -> 
    ei -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
roundedRefinementIsotoneDom _ contextDescription exprUp exprDn (RefOrd.LEPair (domDn, domUp)) effort effortComp =
--    unsafePrint
--    (
--        "roundedRefinementIsotoneDom: "
--        ++ "\n domUp = " ++ show domUp
--        ++ "\n domDn = " ++ show domDn
--        ++ "\n resUp = " ++ show resUp
--        ++ "\n resDn = " ++ show resDn
--    ) $
    case RefOrd.pLeqEff effortComp resDn resUp of
        Just True -> True
        Just False -> 
            unsafePrint
            (
                contextDescription ++ ": roundedRefinementIsotoneDom failed for:"
                ++ "\n domUp = " ++ show domUp
                ++ "\n domDn = " ++ show domDn
                ++ "\n resUp = " ++ show resUp
                ++ "\n resDn = " ++ show resDn
            )
            False
        _ -> True
    where
    resUp = check $ exprUp effort domUp
    resDn = check $ exprDn effort domDn
    check = id -- detectIllegalValues $ contextDescription ++ " refinement isotone"
    
    
{-  -}    
propEvalRefIsotone ::
    (CanEvaluate f,
     RefOrd.PartialComparison (Domain f),
     RefOrd.ArbitraryOrderedTuple (VarBox f (Domain f)), 
     Area (VarBox f (Domain f)) ~ VarBox f (Domain  f),
     Show f,
     Show (Domain f),
     Show (DomainBox f))
    =>
    f ->
    (SingletonInArea f) -> 
    (Area (DomainBox f),
        (RefOrd.LEPair (DomainBox f)) -> 
        EvaluationEffortIndicator f ->
        (RefOrd.PartialCompareEffortIndicator (Domain f)) ->
        Bool)
propEvalRefIsotone _
        (SingletonInArea fn)
    =
--    unsafePrint
--    (
--       "propEvalRefIsotone: "
--       ++ "\n fn = " ++ show fn
--    ) $
    (domBox, roundedRefinementIsotoneDom fn "function evaluation" evalIn evalOut) 
    where
    domBox = getDomainBox fn
    evalIn eff d = evalAtPointInEff eff d fn
    evalOut eff d = evalAtPointOutEff eff d fn

testsEval ::
    (Show f,
     Show (Domain f),
     Show (VarBox f (Domain f)),
     ArbitraryWithArea f,
     RefOrd.ArbitraryOrderedTuple (VarBox f (Domain f)),
     RefOrd.PartialComparison (Domain f),
     CanEvaluate f,
     Area (DomainBox f) ~ (DomainBox f))
     =>
    ([Char], f) -> Area f -> Test
testsEval (name, sample) area =
    testGroup (name ++ " evaluation") $
        [
            testProperty "refinement isotone" (area, propEvalRefIsotone sample)
        ]
    
propNumCompareAgreesWithPointwise :: 
    (Show f,
     Show (DomainBox f),
     NumOrd.PartialComparison f,
     NumOrd.PartialComparison (Domain f),
     CanEvaluate f)
    =>
    f -> 
    PairInArea f ->
    (DomainBox f,
        (SingletonInArea (DomainBox f)) -> 
        NumOrd.PartialCompareEffortIndicator f -> 
        NumOrd.PartialCompareEffortIndicator (Domain f) -> 
        EvaluationEffortIndicator f -> 
        Bool)
propNumCompareAgreesWithPointwise sampleFn (PairInArea fns) =
    (domBox, roundedAgreeWithPointwisePred2 sampleFn  "function numerical comparison" NumOrd.pLeqEff NumOrd.pLeqEff fns)
    where
    domBox = getDomainBox sampleFn
    
testsFnNumCompare :: 
    (Show f,
     Show (DomainBox f),
     ArbitraryWithArea (DomainBox f),
     NumOrd.PartialComparison f,
     NumOrd.PartialComparison (Domain f),
     NumOrd.ArbitraryOrderedTuple f,
     CanEvaluate f,
     Area (DomainBox f) ~ (DomainBox f))
    =>
    ([Char], f) -> Area f -> Test
testsFnNumCompare (name, sample) area =
    testGroup (name ++ " numerical comparison") $
        [
            testProperty "pointwise" (area, propNumCompareAgreesWithPointwise sample)
        ]
    