{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax
    Description :  approximation of min and max using only ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximation of min and max using only ring operations.
    .
    The motivating use case for this module is where we compute min or max for a 
    /function/ pointwise over its domain.
-}

module Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.RefinementOrderRounding.BernsteinPoly

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort
----import Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Exception
import Control.Exception
--import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.Mutable
--import Numeric.AERN.RealArithmetic.ExactOps

--import Test.QuickCheck

--import qualified Data.List as List

--import Control.Monad.ST (ST)

import Numeric.AERN.Misc.Debug
_ = unsafePrint

type MinmaxEffortIndicatorFromRingOps f t =
        ((ArithUpDn.ConvertEffortIndicator t (Domain f), -- finding the range of a function of type t
          ArithInOut.RoundedRealEffortIndicator (Domain f),
          RefOrd.GetEndpointsEffortIndicator (Domain f)
          )
          ,
          (EvalOpsEffortIndicator f t,
           EvalOpsEffortIndicator f (Domain f))
          ,
         (ArithInOut.RingOpsEffortIndicator f,
          ArithInOut.MixedFieldOpsEffortIndicator f Int,
          SizeLimits f
         ),
         (ArithInOut.RingOpsEffortIndicator t,
          ArithInOut.MixedFieldOpsEffortIndicator t (Domain f)
         )
        )

--deriving instance
--    (ArithUpDn.Convertible t (Domain f),
--     ArithInOut.RoundedReal (Domain f),
--     HasEvalOps f t, HasEvalOps f (Domain f),
--     ArithInOut.RoundedRing t,
--     ArithInOut.RoundedRing f,
--     ArithInOut.RoundedMixedField f Int,
--     ArithInOut.RoundedMixedField t (Domain f),
--     Show (SizeLimits f)) 
--    =>
--    Show (MinmaxEffortIndicatorFromRingOps f t)

    
--minmaxEffortIndicatorFromRingOpsAdjustDegree ::
--    MinmaxEffortIndicatorFromRingOps f t ->
--    (Int -> Int) ->
--    MinmaxEffortIndicatorFromRingOps f t
--minmaxEffortIndicatorFromRingOpsAdjustDegree effMinmax@(a,b,(c1,c2,c3,Int1To10 degree),d) adjDegree =
--    (a,b,(c1,c2,c3, Int1To10 $ adjDegree degree),d)

defaultMinmaxEffortIndicatorFromRingOps :: 
    (ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     HasSizeLimits f,
     HasDomainBox f,
     HasEvalOps f t,
     HasEvalOps f (Domain f),
     ArithInOut.RoundedRingEffort f,
     ArithInOut.RoundedMixedFieldEffort f Int,
     ArithInOut.RoundedMixedField t (Domain f),
     ArithInOut.RoundedRingEffort t
    )
    =>
    f {-^ an arbitrary sample value of a function type used to model internal Bernstein approximations -} -> 
    t {-^ an arbitrary sample value of the main type -} -> 
    MinmaxEffortIndicatorFromRingOps f t
defaultMinmaxEffortIndicatorFromRingOps sampleF sampleT =
    ((ArithUpDn.convertDefaultEffort sampleT sampleDF, -- finding the range of a function of type t
      ArithInOut.roundedRealDefaultEffort sampleDF,
      RefOrd.getEndpointsDefaultEffort sampleDF
     )
     ,
     (evalOpsDefaultEffort sampleF sampleT,
      evalOpsDefaultEffort sampleF sampleDF
     )
     ,
     (ArithInOut.ringOpsDefaultEffort sampleF,
      ArithInOut.mixedFieldOpsDefaultEffort sampleF (1::Int),
      getSizeLimits sampleF
     )
     ,
     (ArithInOut.ringOpsDefaultEffort sampleT,
      ArithInOut.mixedFieldOpsDefaultEffort sampleT sampleDF
     )
    )
    where
    sampleDF = getSampleDomValue sampleF

maxUpEffFromRingOps :: 
    (
     Show t, Show f,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     RefOrd.IntervalLike t,
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    f ->
    (SizeLimits f -> f) ->
    MinmaxEffortIndicatorFromRingOps f t ->
    Int {- ^ degree of Bernstein approximations (must be > 1) -} ->
    t -> t -> (t, t)
maxUpEffFromRingOps _ getX eff@(_, _, _, (effRing, effFieldTDF)) degree a b =
    (upper, upperShiftedBelow)
    where
    upperShiftedBelow =
        let ?mixedAddInOutEffort = effAddTDF in
        upper <+>| (neg errUp)
    upper =
        let ?addInOutEffort = effAdd in
        a <+> maxBMinusAUp 
    ((_, maxBMinusAUp), errUp) = 
        let ?addInOutEffort = effAdd in
        maxZeroDnUp getX eff degree $ b <-> a
    effAdd = ArithInOut.ringEffortAdd sampleT $ effRing
    effAddTDF = ArithInOut.mxfldEffortAdd sampleT sampleDF $ effFieldTDF
    sampleT = a
    sampleDF = errUp

maxDnEffFromRingOps :: 
    (
     Show t, Show f,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     RefOrd.IntervalLike t,
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    f ->
    (SizeLimits f -> f) ->
    MinmaxEffortIndicatorFromRingOps f t -> 
    Int {- ^ degree of Bernstein approximations (must be > 1) -} ->
    t -> t -> t
maxDnEffFromRingOps _ getX eff@(_, _, _, (effRing, _)) degree a b =
    let ?addInOutEffort = effAdd in
    a <+> (fst $ fst $ maxZeroDnUp getX eff degree $ b <-> a)
    where
    effAdd = ArithInOut.ringEffortAdd sampleT $ effRing
    sampleT = a

minUpEffFromRingOps ::
    (
     Show t, Show f,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     RefOrd.IntervalLike t,
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    f ->
    (SizeLimits f -> f) ->
    MinmaxEffortIndicatorFromRingOps f t -> 
    Int {- ^ degree of Bernstein approximations (must be > 1) -} ->
    t -> t -> t
minUpEffFromRingOps sampleF getX eff degree a b =
    neg $ maxDnEffFromRingOps sampleF getX eff degree (neg a) (neg b)

minDnEffFromRingOps ::
    (
     Show t, Show f,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     RefOrd.IntervalLike t,
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    f ->
    (SizeLimits f -> f) ->
    MinmaxEffortIndicatorFromRingOps f t -> 
    Int {- ^ degree of Bernstein approximations (must be > 1) -} ->
    t -> t -> (t, t)
minDnEffFromRingOps sampleF getX eff degree a b =
    (neg resNeg, neg resShiftedNeg)
    where
    (resNeg, resShiftedNeg) =
        maxUpEffFromRingOps sampleF getX eff degree (neg a) (neg b) 

maxZeroDnUp ::    
    (
     Show t, Show f,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     RefOrd.IntervalLike t,
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    (SizeLimits f -> f) ->
    MinmaxEffortIndicatorFromRingOps f t ->
    Int {- ^ degree of Bernstein approximations (must be > 1) -} ->
    t -> 
    ((t,t), Domain f)
{-
    overview of the algorithm:
    
    * find bounds of type (Domain f) for a (a is of type t)
    * if a can be shown to be positive or negative, finish
    * affinely transform a to a' so that it fits inside [0,1] 
      and note the point 0 < c < 1 where the original crossed zero
    * compute a Bernstein approximation of the given degree to the function \y -> \max(0,y-c)
    * compute a reliable estimate e of the approximation error
    * r' = evaluate this polynomial with a' for y and add [-e,0] 
    * transform r' back to the range of a to get the result r 
-}
maxZeroDnUp
        getX
        ((effTToDom, effRealDF, effGetEDF), 
         (effEvalOpsT, effEvalOpsDF), 
         (effRingF, effIntFldF, sizeLimits), 
         (_effRingT, effFldTDF))
        degree
        a =
    let ?pCompareEffort = effCompDF in
    case (bounded, maybeaDn, c0 <=? aDn, maybeaUp, aUp <=? c0) of
        (_,Nothing, _,_,_) -> 
            throw $ AERNException $ "maxZeroDnUp called for an unbounded value: " ++ show a
        (_,_,_,Nothing,_) -> 
            throw $ AERNException $ "maxZeroDnUp called for an unbounded value: " ++ show a
        (False,_,_,_,_) -> 
            throw $ AERNException $ "maxZeroDnUp called for an unbounded value: " ++ show a
        (_,_, Just True, _, _) ->
--            unsafePrint ("maxZeroDnUp: positive") $ 
            ((a, a), zero sampleDF)
        (_,_,_,_, Just True) -> 
--            unsafePrint ("maxZeroDnUp: negative") $ 
            ((zero a, zero a), zero sampleDF)
        _ -> 
--            unsafePrint ("maxZeroDnUp: mixed"
--                ++ "\n maxCUp = " ++ show maxCUp
--                ++ "\n translateToUnit a = " ++ show (translateToUnit a)
--                ++ "\n evalOtherType (evalOpsOut effEvalOpsT sampleF sampleT) (fromAscList [(var, translateToUnit a)]) maxZeroUp = " 
--                ++ show (evalOtherType (evalOpsOut effEvalOpsT sampleF sampleT) (fromAscList [(var, translateToUnit a)]) maxZeroUp)
--                ++ "\n maxCDn = " ++ show maxCDn
--            ) $ 
            ((viaBernsteinDn, viaBernsteinUp), errUp)
    where
    sampleT = a
    sampleF = x
    x = getX sizeLimits
    sampleDF = getSampleDomValue x
    maybeaUp = ArithUpDn.convertUpEff effTToDom sampleDF a
    maybeaDn = ArithUpDn.convertDnEff effTToDom sampleDF a
    Just aUp = maybeaUp
    Just aDn = maybeaDn
    bounded = excludesInfinity aWidth
    
    viaBernsteinUp = 
        doSubst $ maxCUp
    viaBernsteinDn = 
        doSubst $ maxCDn
    doSubst p =  
        translateFromUnit $
        evalOtherType (evalOpsEff effEvalOpsT sampleF sampleT) varA p
        where
        varA = fromAscList [(var, translateToUnit a)]
    (var:_) = getVars $ getDomainBox $ x
    errUp = 
        let ?multInOutEffort = effMultDF in
        errUpUnit <*> aWidth
    (maxCUp, errUpUnit) = 
        hillbaseApproxUp effCompDF effRingF effIntFldF effRealDF effEvalOpsDF x c degree
    maxCDn =
        hillbaseApproxDn effGetEDF effCompDF effRingF effIntFldF effRealDF effEvalOpsDF x c dInit degree
        where
        dInit = 
            let (<->) = ArithInOut.subtrOutEff effAddDF in
--            let (</>|) = ArithInOut.mixedDivOutEff effDivDFI in
            let (<*>|) = ArithInOut.mixedMultOutEff effMultDFI in
            (maxCUpAtC <-> c)
                  <*>| (2 :: Int)
--                  </>| (2 :: Int)
        maxCUpAtC =
            evalOtherType (evalOpsEff effEvalOpsDF sampleF sampleDF) varC maxCUp
        varC = fromAscList [(var, c)]
    c = 
        let (</>) = ArithInOut.divOutEff effDivDF in
        (neg aDn) </> aWidth
    aWidth = 
        let (<->) = ArithInOut.subtrOutEff effAddDF in
        aUp <-> aDn
    translateToUnit b =
        let (<+>|) = ArithInOut.mixedAddOutEff effAddTDF in
        let (</>|) = ArithInOut.mixedDivOutEff effDivTDF in
        (b <+>| (neg aDn)) </>| aWidth
    translateFromUnit b =
        let (<+>|) = ArithInOut.mixedAddOutEff effAddTDF in
        let (<*>|) = ArithInOut.mixedMultOutEff effMultTDF in
        (b <*>| aWidth) <+>| aDn
    c0 = zero sampleDF
    _ = [c,c0]
    
    effCompDF = ArithInOut.rrEffortNumComp c0 effRealDF
    effAddDF = ArithInOut.fldEffortAdd c0 $ ArithInOut.rrEffortField c0 effRealDF
    effMultDF = ArithInOut.fldEffortMult c0 $ ArithInOut.rrEffortField c0 effRealDF
    effDivDF = ArithInOut.fldEffortDiv c0 $ ArithInOut.rrEffortField c0 effRealDF
    
    effAddTDF = ArithInOut.mxfldEffortAdd a c0 effFldTDF
    effMultTDF = ArithInOut.mxfldEffortMult a c0 effFldTDF
    effDivTDF = ArithInOut.mxfldEffortDiv a c0 effFldTDF
    
    effMultDFI = ArithInOut.mxfldEffortMult sampleDF (1::Int) $ ArithInOut.rrEffortIntMixedField sampleDF effRealDF
--    effDivDFI = ArithInOut.mxfldEffortDiv sampleDF (1::Int) $ ArithInOut.rrEffortIntMixedField sampleDF effRealDF

{-| compute an upper Bernstein approximation of the function max(x,c) over [0,1] -}
hillbaseApproxUp :: 
    (HasConstFns f, HasProjections f, HasOne f, ArithInOut.RoundedRing f, 
     ArithInOut.RoundedMixedField f Int,
     ArithInOut.RoundedReal (Domain f),
     HasEvalOps f (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show (Domain f), Show f)
    =>
    NumOrd.PartialCompareEffortIndicator (Domain f) -> 
    ArithInOut.RingOpsEffortIndicator f -> 
    ArithInOut.MixedFieldOpsEffortIndicator f Int -> 
    ArithInOut.RoundedRealEffortIndicator (Domain f) -> 
    EvalOpsEffortIndicator f (Domain f) -> 
    f {-^ the variable @x@ to use in the result uni-variate polynomial -} ->
    Domain f {-^ @c@ the only non-smooth point of the approximated piece-wise linear function -} ->
    Int {-^ @n@ Bernstein approximation degree -} ->
    (f, Domain f)
hillbaseApproxUp effComp effRingF effIntFldF effRealDF effEvalOps x c n =
--    unsafePrintReturn ( "hillbaseApproxUp:"
--        ++ "\n c = " ++ show c
--        ++ "\n result = "
--    ) $
    (result, errUp)
    where
    result =
        let ?pCompareEffort = effComp in
        let ?addInOutEffort = effAddDF in
        let ?multInOutEffort = effMultF in
        let ?mixedMultInOutEffort = effMultDFI in
        let ?mixedDivInOutEffort = effDivDFI in
        foldl1 (ArithInOut.addOutEff effAddF) $
            map mkBT [0..n]
        
    errUp = 
        let ?addInOutEffort = effAddDF in
        valueAtC <-> c
        where
        valueAtC =
            evalOtherType (evalOpsEff effEvalOps sampleF sampleDF) varC result
        varC = fromAscList [(var, c)]
    mkBT p =
        (newConstFnFromSample x fOfpOverN)
        <*> 
        (bernsteinOut (effRingF, effIntFldF) x n p)
        where
        fOfpOverN -- = maxOutEff effMinmax c0 $ pOverN <-> c
            | (pOverN <? c) == Just True = c
            | otherwise = pOverN
        pOverN = (c1 <*>| p) </>| n
    c1 = one sampleDF
    sampleDF = getSampleDomValue x
    sampleF = x
    (var:_) = getVars $ getDomainBox $ x
    
    effAddF = ArithInOut.ringEffortAdd x effRingF
    effMultF = ArithInOut.ringEffortMult x effRingF
    
    effMultDFI = ArithInOut.mxfldEffortMult sampleDF (1::Int) $ ArithInOut.rrEffortIntMixedField sampleDF effRealDF
    effDivDFI = ArithInOut.mxfldEffortDiv sampleDF (1::Int) $ ArithInOut.rrEffortIntMixedField sampleDF effRealDF

--    effMultDF = ArithInOut.fldEffortMult sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF
    effAddDF = ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF

{-| compute a lower Bernstein approximation of the function max(c,x) over [0,1] 
    This needs to be done by choosing the control points somewhat below the exact
    answer.  To work out how much below we search for a close-to-optimal parameter
    for the hillbaseApproxDnD function.  We expect the @dInit@ value to be larger
    than necessary.  First we recursively decrease this d until it below the safe limit.
    Then we recursively increase it to make a little above the safe limit value. 
-}
hillbaseApproxDn :: 
    (HasConstFns f, HasProjections f, HasOne f, ArithInOut.RoundedRing f, 
     ArithInOut.RoundedMixedField f Int,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     HasEvalOps f (Domain f),
     Show (Domain f), Show f)
    =>
    RefOrd.GetEndpointsEffortIndicator (Domain f) -> 
    NumOrd.PartialCompareEffortIndicator (Domain f) -> 
    ArithInOut.RingOpsEffortIndicator f -> 
    ArithInOut.MixedFieldOpsEffortIndicator f Int -> 
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    EvalOpsEffortIndicator f (Domain f) -> 
    f {-^ the variable @x@ to use in the result uni-variate polynomial -} ->
    Domain f {-^ @c@ the only non-smooth point of the approximated piece-wise linear function -} ->
    Domain f {-^ @dInit@ initial value for the offset @d@ by which to translate the approximated fn down at point c -} ->
    Int {-^ @n@ Bernstein approximation degree -} ->
    f
hillbaseApproxDn effGetE effComp effRingF effIntFldF effRealDF effEvalOps x c dInit n =
    let ?addInOutEffort = effAddDF in
    let ?mixedMultInOutEffort = effMultDFDbl in
    findDWithBelowCAtC $ findDWithAboveCAtC dInit
    where
    findDWithAboveCAtC d =
        let ?pCompareEffort = effComp in
        case (fnAtCRE <=? c) of
            Just False -> d
            _ -> findDWithAboveCAtC newD
        where
        (_, fnAtCRE) =
            RefOrd.getEndpointsOutEff effGetE fnAtC
        fnAtC = evalOtherType (evalOpsEff effEvalOps sampleF sampleDF) varC fn
            where
            varC = fromAscList [(var, c)]
        fn = hillDnD d
        (newD, _) = -- round downwards because we are trying to decrease d below safe limit
             RefOrd.getEndpointsOutEff effGetE $
                d <+> (fnAtCMinusC <*>| (1.25 :: Double)) -- get nearer the optimal value of d
            where
            fnAtCMinusC = fnAtC <-> c
    findDWithBelowCAtC d =
        let ?pCompareEffort = effComp in
        case (fnAtCLE <=? c) of
            Just True -> fn
            _ -> findDWithBelowCAtC newD
        where
        (fnAtCLE,_) =
            RefOrd.getEndpointsOutEff effGetE fnAtC
        fnAtC = evalOtherType (evalOpsEff effEvalOps sampleF sampleDF) varC fn
            where
            varC = fromAscList [(var, c)]
        fn = hillDnD d
        (_, newD) = -- round upwards because we are trying to increase d above safe limit
             RefOrd.getEndpointsOutEff effGetE $
                d <+> (fnAtCMinusC <*>| (1.25 :: Double)) -- get nearer the optimal value of d
            where
            fnAtCMinusC = fnAtC <-> c
    hillDnD = 
        hillbaseApproxDnD effComp effRingF effIntFldF effRealDF x c n
    
    (var:_) = getVars $ getDomainBox $ x
    sampleF = x
    sampleDF = getSampleDomValue x
    effAddDF = ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF
    effMultDFDbl = ArithInOut.mxfldEffortMult sampleDF (1::Double) $ ArithInOut.rrEffortDoubleMixedField sampleDF effRealDF

{-| 
  Compute an upper Bernstein approximation of the function @max(c-l-x(d-l)/c,x-r-(1-x)(d-r)/(1-c))@ over @[0,1]@,
  which is a valid lower approximation of @max(c,x)@ when @l,d,r@ are large enough.
  @l,d,r@ are the downwards offsets of the control points at @0,c,1@, respectively. 
-}
hillbaseApproxDnD :: 
    (HasConstFns f, HasProjections f, HasOne f, ArithInOut.RoundedRing f, 
     ArithInOut.RoundedMixedField f Int,
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f), Show f)
    =>
    NumOrd.PartialCompareEffortIndicator (Domain f) -> 
    ArithInOut.RingOpsEffortIndicator f -> 
    ArithInOut.MixedFieldOpsEffortIndicator f Int -> 
    ArithInOut.RoundedRealEffortIndicator (Domain f) -> 
    f {-^ the variable @x@ to use in the result uni-variate polynomial -} ->
    Domain f {-^ @c@ the only non-smooth point of the approximated piece-wise linear function -} ->
    Int {-^ @n@ Bernstein approximation degree -} ->
    Domain f {-^ @d@ the distance of the approximated piece-wise linear function from 0 at point c -} ->
    f
hillbaseApproxDnD effComp effRingF effIntFldF effRealDF x c n d =
    let ?pCompareEffort = effComp in
    let ?addInOutEffort = effAddDF in
    let ?multInOutEffort = effMultDF in
    let ?mixedMultInOutEffort = effMultDFI in
    let ?mixedDivInOutEffort = effDivDFI in
--    unsafePrint ( "hillbaseApproxDnD:"
--        ++ " n = " ++ show n
--        ++ " c = " ++ show c
--        ++ " d = " ++ show d
--    ) $
    foldl1 (ArithInOut.addOutEff effAddF) $
        map mkBT [0..n]
    where
    l = 
        let ?multInOutEffort = effMultDF in
        let ?addInOutEffort = effAddDF in  
        let ?intPowerInOutEffort = effPowDF in
        d <*> ((c1 <-> c) <^> (4 :: Int))
    r = 
        let ?multInOutEffort = effMultDF in
        let ?intPowerInOutEffort = effPowDF in
        d <*> (c <^> (4 :: Int))
    mkBT p =
        let ?multInOutEffort = effMultF in
        (newConstFnFromSample x fOfpOverN)
        <*> 
        (bernsteinOut (effRingF, effIntFldF) x n p)
        where
        fOfpOverN
            | (pOverN <? c) == Just True =
                cMinusL <-> (pOverN <*> dMinusLOverC)
                -- (c-l) - (p/n(d-l)/c) 
            | otherwise =
                pOverN <*> onePlusDMinusROverOneMinusC <-> rPlusDMinusROverOneMinusC
                -- (p/n)(1 + (d-r)/(1-c)) - (r + (d-r)/(1-c))
        pOverN = (c1 <*>| p) </>| n
    cMinusL =
        let ?addInOutEffort = effAddDF in  
        c <-> l
    dMinusLOverC = 
        let ?addInOutEffort = effAddDF in  
        let ?divInOutEffort = effDivDF in  
        (d <-> l) </> c
    onePlusDMinusROverOneMinusC = 
        let ?addInOutEffort = effAddDF in  
        c1 <+> dMinusROverOneMinusC
    rPlusDMinusROverOneMinusC = 
        let ?addInOutEffort = effAddDF in  
        r <+> dMinusROverOneMinusC
    dMinusROverOneMinusC = 
        let ?addInOutEffort = effAddDF in  
        let ?divInOutEffort = effDivDF in  
        (d <-> r) </> (c1 <-> c)

    c1 = one sampleDF
    sampleDF = getSampleDomValue x
    
    effAddF = ArithInOut.ringEffortAdd x effRingF
    effMultF = ArithInOut.ringEffortMult x effRingF
    
    effMultDFI = ArithInOut.mxfldEffortMult sampleDF (1::Int) $ ArithInOut.rrEffortIntMixedField sampleDF effRealDF
    effDivDFI = ArithInOut.mxfldEffortDiv sampleDF (1::Int) $ ArithInOut.rrEffortIntMixedField sampleDF effRealDF

    effAddDF = ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF
    effMultDF = ArithInOut.fldEffortMult sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF
    effPowDF = ArithInOut.fldEffortPow sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF
    effDivDF = ArithInOut.fldEffortDiv sampleDF $ ArithInOut.rrEffortField sampleDF effRealDF


    
    

    