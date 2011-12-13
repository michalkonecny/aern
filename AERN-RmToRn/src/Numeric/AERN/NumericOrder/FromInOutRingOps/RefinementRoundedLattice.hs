{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.FromInOutRingOps.RefinementRoundedLattice
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

module Numeric.AERN.NumericOrder.FromInOutRingOps.RefinementRoundedLattice where

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

import Control.Monad.ST (ST)

type MinmaxInOutEffortIndicatorFromRingOps f t =
    ((ArithUpDn.ConvertEffortIndicator t (Domain f), -- finding the range of a function of type t
      ArithInOut.RoundedRealEffortIndicator (Domain f),
      Int, -- ^ degree of Bernstein approximations
      f), -- ^ variable x over [0,1] of the function type @f@ to use for computing Bernstein approximation of max(0,x-c)
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
      x), -- ^ variable x over [0,1] of the function type @f@ to use for computing Bernstein approximation of max(0,x-c)
     (ArithInOut.ringOpsDefaultEffort sampleF,
      ArithInOut.mixedFieldOpsDefaultEffort sampleF (1::Int)
     ),
     (ArithInOut.ringOpsDefaultEffort sampleT,
      ArithInOut.mixedFieldOpsDefaultEffort sampleT sampleDF
     )
    )
    where
    sampleDF = getSampleDomValue sampleF
    

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
    
maxZeroUp ::    
    (
     HasZero t, 
     ArithInOut.RoundedRing t,
     ArithUpDn.Convertible t (Domain f), 
     ArithInOut.RoundedReal (Domain f),
     ArithInOut.RoundedMixedField t (Domain f),
     HasEvalOps f t, 
     HasVarValue (VarBox f t) (Var f) t,
     HasProjections f, HasConstFns f, HasOne f, -- HasZero f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
    =>
    MinmaxInOutEffortIndicatorFromRingOps f t -> t -> t
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
maxZeroUp ((effTToDom, effDomReal, degree, x), (effRingF, effIntFldF), (effRingT, effFldTDF)) a =
    let ?pCompareEffort = effCompDF in
    case (maybeaDn, c0 <=? aDn, maybeaUp, aUp <=? c0) of
        (Nothing, _,_,_) -> error "maxZeroUp called for an unbounded value"
        (_,_,Nothing,_) -> error "maxZeroUp called for an unbounded value"
        (_, Just True, _, _) -> a
        (_,_,_, Just True) -> zero a
        _ -> viaBernstein
    where
    sampleT = a
    sampleF = x
    sampleDF = getSampleDomValue x
    maybeaUp = ArithUpDn.convertUpEff effTToDom a
    maybeaDn = ArithUpDn.convertDnEff effTToDom a
    Just aUp = maybeaUp
    Just aDn = maybeaDn
    viaBernstein =
        translateFromUnit $
        evalOtherType (evalOpsOut sampleF sampleT) varBox $
            hillbaseApproxUp effCompDF effRingF effIntFldF effDomReal x c degree
        where
        varBox = fromAscList [(var, translateToUnit a)]
        (var:_) = getVars $ getDomainBox $ x
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


    
    

    