{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

--import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort
----import Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Data.List as List

import Control.Monad.ST (ST)

type MinmaxInOutEffortIndicatorFromRingOps f t =
    ((ArithUpDn.ConvertEffortIndicator t (Domain f), -- finding the range of a function of type t
      ArithInOut.RoundedRealEffortIndicator (Domain f),
      Int, -- ^ degree of Bernstein approximations
      f) -- ^ variable x over [0,1] of the function type @f@ to use for computing Bernstein approximation of max(0,x-c)
      ,
      (EvalOpsEffortIndicator f t,
       EvalOpsEffortIndicator f (Domain f))
      ,
     (ArithInOut.RingOpsEffortIndicator f,
      ArithInOut.MixedFieldOpsEffortIndicator f Int
     ),
     (ArithInOut.RingOpsEffortIndicator t,
      ArithInOut.MixedFieldOpsEffortIndicator t (Domain f)
     )
    )
     
defaultMinmaxInOutEffortIndicatorFromRingOps :: 
    (ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     HasDomainBox f,
     HasEvalOps f t,
     HasEvalOps f (Domain f),
     ArithInOut.RoundedRingEffort f,
     ArithInOut.RoundedMixedFieldEffort f Int,
     ArithInOut.RoundedMixedField t (Domain f),
     ArithInOut.RoundedRingEffort t
    )
    =>
    f {-^ the identity function over interval [0,1] in the type used for approximating Bernstein polynomials -} -> 
    t {-^ an arbitrary sample value of the main type -} -> 
    MinmaxInOutEffortIndicatorFromRingOps f t
defaultMinmaxInOutEffortIndicatorFromRingOps =
    defaultMinmaxInOutEffortIndicatorFromRingOpsDegree 2

defaultMinmaxInOutEffortIndicatorFromRingOpsDegree :: 
    (ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     HasDomainBox f,
     HasEvalOps f t,
     HasEvalOps f (Domain f),
     ArithInOut.RoundedRingEffort f,
     ArithInOut.RoundedMixedFieldEffort f Int,
     ArithInOut.RoundedMixedField t (Domain f),
     ArithInOut.RoundedRingEffort t
    )
    =>
    Int ->
    f {-^ the identity function over interval [0,1] in the type used for approximating Bernstein polynomials -} -> 
    t {-^ an arbitrary sample value of the main type -} -> 
    MinmaxInOutEffortIndicatorFromRingOps f t
defaultMinmaxInOutEffortIndicatorFromRingOpsDegree degree sampleF@x sampleT =
    ((ArithUpDn.convertDefaultEffort sampleT sampleDF, -- finding the range of a function of type t
      ArithInOut.roundedRealDefaultEffort sampleDF,
      degree, -- ^ degree of Bernstein approximations
      x) -- ^ variable @x@ over @[0,1]@ of the function type @f@ to use for computing Bernstein approximation of @max(0,x-c)@
     ,
     (evalOpsDefaultEffort sampleF sampleT,
      evalOpsDefaultEffort sampleF sampleDF
     )
     ,
     (ArithInOut.ringOpsDefaultEffort sampleF,
      ArithInOut.mixedFieldOpsDefaultEffort sampleF (1::Int)
     ),
     (ArithInOut.ringOpsDefaultEffort sampleT,
      ArithInOut.mixedFieldOpsDefaultEffort sampleT sampleDF
     )
    )
    where
    sampleDF = getSampleDomValue sampleF

maxUpEffFromRingOps :: 
    (
     Show t,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    MinmaxInOutEffortIndicatorFromRingOps f t -> 
    t -> t -> t
maxUpEffFromRingOps eff@(_, _, _, (effRing, _)) a b =
    let ?addInOutEffort = effAdd in
    a <+> (snd $ maxZeroDnUp eff $ b <-> a)
    where
    effAdd = ArithInOut.ringEffortAdd sampleT $ effRing
    sampleT = a

maxDnEffFromRingOps :: 
    (
     Show t,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    MinmaxInOutEffortIndicatorFromRingOps f t -> 
    t -> t -> t
maxDnEffFromRingOps eff@(_, _, _, (effRing, _)) a b =
    let ?addInOutEffort = effAdd in
    a <+> (fst $ maxZeroDnUp eff $ b <-> a)
    where
    effAdd = ArithInOut.ringEffortAdd sampleT $ effRing
    sampleT = a

--maxOutEffFromRingOps :: 
--    (ArithInOut.RoundedAdd t, 
--     ArithInOut.RoundedSubtr t, 
--     ArithInOut.RoundedMultiply t, 
--     ArithInOut.RoundedPowerToNonnegInt t) =>
--    MinmaxInOutEffortIndicatorFromRingOps f t -> t -> t -> t
--maxOutEffFromRingOps eff@(_, _, effAdd, _) a b =
--    let ?addInOutEffort = effAdd in
--    a <+> (maxZeroOut eff $ b <-> a)
--
--maxZeroOut ::    
--    (ArithInOut.RoundedAdd t, 
--     ArithInOut.RoundedSubtr t, 
--     ArithInOut.RoundedMultiply t, 
--     ArithInOut.RoundedPowerToNonnegInt t) =>
--    MinmaxInOutEffortIndicatorFromRingOps f t -> t -> t
--maxZeroOut (degree, x, effAdd, effMult) a =
--    error $ "maxZero not implemented yet"
    
maxZeroDnUp ::    
    (
     Show t,
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     Show (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     HasVarValue (VarBox f t) (Var f) t,
     HasEvalOps f (Domain f),
     HasVarValue (VarBox f (Domain f)) (Var f) (Domain f),
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    MinmaxInOutEffortIndicatorFromRingOps f t -> 
    t -> 
    (t,t)
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
        ((effTToDom, effDomReal, degree, x), 
         (effEvalOpsT, effEvalOpsDF), 
         (effRingF, effIntFldF), 
         (effRingT, effFldTDF)) 
        a =
    let ?pCompareEffort = effCompDF in
    case (bounded, maybeaDn, c0 <=? aDn, maybeaUp, aUp <=? c0) of
        (_,Nothing, _,_,_) -> error "maxZeroUp called for an unbounded value"
        (_,_,_,Nothing,_) -> error "maxZeroUp called for an unbounded value"
        (False,_,_,_,_) -> error "maxZeroUp called for an unbounded value"
        (_,_, Just True, _, _) -> (a, a)
        (_,_,_,_, Just True) -> (zero a, zero a)
        _ -> (viaBernsteinDn, viaBernsteinUp)
    where
    sampleT = a
    sampleF = x
    sampleDF = getSampleDomValue x
    maybeaUp = ArithUpDn.convertUpEff effTToDom a
    maybeaDn = ArithUpDn.convertDnEff effTToDom a
    Just aUp = maybeaUp
    Just aDn = maybeaDn
    bounded = excludesInfinity aWidth  
    
    viaBernsteinUp = doSubst maxZeroUp
    viaBernsteinDn = doSubst maxZeroDn
    doSubst p =  
        translateFromUnit $
        evalOtherType (evalOpsOut effEvalOpsT sampleF sampleT) varA p
        where
        varA = fromAscList [(var, translateToUnit a)]
    (var:_) = getVars $ getDomainBox $ x
    maxZeroUp = hillbaseApproxUp effCompDF effRingF effIntFldF effDomReal x c degree
    maxZeroDn =
        hillbaseApproxDn effCompDF effRingF effIntFldF effDomReal effEvalOpsDF x c dInit degree
        where
        dInit = maxZeroUpAtC
        maxZeroUpAtC =
            evalOtherType (evalOpsOut effEvalOpsDF sampleF sampleDF) varC maxZeroUp
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
    
    effCompDF = ArithInOut.rrEffortNumComp c0 effDomReal
    effAddDF = ArithInOut.fldEffortAdd c0 $ ArithInOut.rrEffortField c0 effDomReal
    effDivDF = ArithInOut.fldEffortDiv c0 $ ArithInOut.rrEffortField c0 effDomReal
    
    effAddTDF = ArithInOut.mxfldEffortAdd a c0 effFldTDF
    effMultTDF = ArithInOut.mxfldEffortMult a c0 effFldTDF
    effDivTDF = ArithInOut.mxfldEffortDiv a c0 effFldTDF
    

{-| compute an upper Bernstein approximation of the function max(0,x-c) over [0,1] -}
hillbaseApproxUp :: 
    (HasConstFns f, HasProjections f, HasOne f, ArithInOut.RoundedRing f, 
     ArithInOut.RoundedMixedField f Int,
     ArithInOut.RoundedReal (Domain f))
    =>
    NumOrd.PartialCompareEffortIndicator (Domain f) -> 
    ArithInOut.RingOpsEffortIndicator f -> 
    ArithInOut.MixedFieldOpsEffortIndicator f Int -> 
    ArithInOut.RoundedRealEffortIndicator (Domain f) -> 
    f {-^ the variable @x@ to use in the result uni-variate polynomial -} ->
    Domain f {-^ @c@ the only non-smooth point of the approximated piece-wise linear function -} ->
    Int {-^ @n@ Bernstein approximation degree -} ->
    f
hillbaseApproxUp effComp effRingF effIntFldF effDomReal x c n =
    let ?pCompareEffort = effComp in
    let ?addInOutEffort = effAddDF in
    let ?multInOutEffort = effMultF in
    let ?mixedMultInOutEffort = effMultDFI in
    let ?mixedDivInOutEffort = effDivDFI in
    foldl1 (ArithInOut.addOutEff effAddF) $
        map mkBT [0..n]
    where
    mkBT p =
        (newConstFnFromSample x fOfpOverN)
        <*> 
        (bernsteinOut (effRingF, effIntFldF) x n p)
        where
        fOfpOverN -- = maxOutEff effMinmax c0 $ pOverN <-> c
            | (pOverN <? c) == Just True = c0
            | otherwise = pOverN <-> c
        pOverN = (c1 <*>| p) </>| n
    c1 = one $ getSampleDomValue x
    c0 = zero $ getSampleDomValue x
    
    effAddF = ArithInOut.ringEffortAdd x effRingF
    effMultF = ArithInOut.ringEffortMult x effRingF
    
    effMultDFI = ArithInOut.mxfldEffortMult c0 (1::Int) $ ArithInOut.rrEffortIntMixedField c0 effDomReal
    effDivDFI = ArithInOut.mxfldEffortDiv c0 (1::Int) $ ArithInOut.rrEffortIntMixedField c0 effDomReal

    effMultDF = ArithInOut.fldEffortMult c0 $ ArithInOut.rrEffortField c0 effDomReal
    effAddDF = ArithInOut.fldEffortAdd c0 $ ArithInOut.rrEffortField c0 effDomReal

{-| compute a lower Bernstein approximation of the function max(0,x-c) over [0,1] -}
hillbaseApproxDn :: 
    (HasConstFns f, HasProjections f, HasOne f, ArithInOut.RoundedRing f, 
     ArithInOut.RoundedMixedField f Int,
     ArithInOut.RoundedReal (Domain f),
     HasEvalOps f (Domain f),
     Show (Domain f))
    =>
    NumOrd.PartialCompareEffortIndicator (Domain f) -> 
    ArithInOut.RingOpsEffortIndicator f -> 
    ArithInOut.MixedFieldOpsEffortIndicator f Int -> 
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    EvalOpsEffortIndicator f (Domain f) -> 
    f {-^ the variable @x@ to use in the result uni-variate polynomial -} ->
    Domain f {-^ @c@ the only non-smooth point of the approximated piece-wise linear function -} ->
    Domain f {-^ @d@ initial value for the offset @d@ by which to translate the approximated fn down at point c -} ->
    Int {-^ @n@ Bernstein approximation degree -} ->
    f
hillbaseApproxDn effComp effRingF effIntFldF effDomReal effEvalOps x c dInit n =
    findBelowZeroAtC approximations
    where
    approximations =
        let ?addInOutEffort = effAddDF in
        getApproxFrom dInit
        where
        getApproxFrom d =
            (fnAtC, fn) : getApproxFrom newD
            where
            fnAtC = evalOtherType (evalOpsOut effEvalOps sampleF sampleDF) varC fn
            fn = hDnD d
            varC = fromAscList [(var, c)]
            newD = d <+> (neg $ fnAtC <+> fnAtC) 
    findBelowZeroAtC ((fnAtC, fn) : rest) =
        let ?pCompareEffort = effComp in
        case (fnAtC <? c0) of
            Just True -> fn
            _ -> findBelowZeroAtC rest
        where
        c0 = zero sampleDF
    hDnD = 
        hillbaseApproxDnD effComp effRingF effIntFldF effDomReal x c n
    
    (var:_) = getVars $ getDomainBox $ x
    sampleF = x
    sampleDF = getSampleDomValue x
    effAddDF = ArithInOut.fldEffortAdd sampleDF $ ArithInOut.rrEffortField sampleDF effDomReal

{-| compute an upper Bernstein approximation of the function max(-xd/c,x-c-(1-x)d/(1-c)) over [0,1] -}
hillbaseApproxDnD :: 
    (HasConstFns f, HasProjections f, HasOne f, ArithInOut.RoundedRing f, 
     ArithInOut.RoundedMixedField f Int,
     ArithInOut.RoundedReal (Domain f))
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
hillbaseApproxDnD effComp effRingF effIntFldF effDomReal x c n d =
    let ?pCompareEffort = effComp in
    let ?addInOutEffort = effAddDF in
    let ?multInOutEffort = effMultDF in
    let ?mixedMultInOutEffort = effMultDFI in
    let ?mixedDivInOutEffort = effDivDFI in
    foldl1 (ArithInOut.addOutEff effAddF) $
        map mkBT [0..n]
    where
    mkBT p =
        let ?multInOutEffort = effMultF in
        (newConstFnFromSample x fOfpOverN)
        <*> 
        (bernsteinOut (effRingF, effIntFldF) x n p)
        where
        fOfpOverN
            | (pOverN <? c) == Just True = 
                pOverN <*> minusDOverC
            | otherwise =
                (pOverN <*> onePlusDOverOneMinusC) <-> cPlusDOverOneMinusC
        pOverN = (c1 <*>| p) </>| n
    minusDOverC = 
        let ?divInOutEffort = effDivDF in  
        neg $ d </> c
    cPlusDOverOneMinusC =
        let ?addInOutEffort = effAddDF in  
        c <+> dOverOneMinusC
    onePlusDOverOneMinusC = 
        let ?addInOutEffort = effAddDF in  
        c1 <+> dOverOneMinusC
    dOverOneMinusC = 
        let ?addInOutEffort = effAddDF in  
        let ?divInOutEffort = effDivDF in  
        d </> (c1 <-> c)
    c1 = one $ getSampleDomValue x
    c0 = zero $ getSampleDomValue x
    
    effAddF = ArithInOut.ringEffortAdd x effRingF
    effMultF = ArithInOut.ringEffortMult x effRingF
    
    effMultDFI = ArithInOut.mxfldEffortMult c0 (1::Int) $ ArithInOut.rrEffortIntMixedField c0 effDomReal
    effDivDFI = ArithInOut.mxfldEffortDiv c0 (1::Int) $ ArithInOut.rrEffortIntMixedField c0 effDomReal

    effMultDF = ArithInOut.fldEffortMult c0 $ ArithInOut.rrEffortField c0 effDomReal
    effAddDF = ArithInOut.fldEffortAdd c0 $ ArithInOut.rrEffortField c0 effDomReal
    effDivDF = ArithInOut.fldEffortDiv c0 $ ArithInOut.rrEffortField c0 effDomReal


    
    

    