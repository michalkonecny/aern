{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Minmax
    Description :  pointwise min/max
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Pointwise min/max of interval polynomials rounding up/down/in/out.
-}

module Numeric.AERN.Poly.IntPoly.Minmax
(
    minmaxUpDnDefaultEffortIntPolyWithBezierDegree,
    minmaxInOutDefaultEffortIntPolyWithBezierDegree
)
where
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()
import Numeric.AERN.Poly.IntPoly.Evaluation ()
import Numeric.AERN.Poly.IntPoly.NumericOrder ()
import Numeric.AERN.Poly.IntPoly.RefinementOrder ()
import Numeric.AERN.Poly.IntPoly.Addition ()
import Numeric.AERN.Poly.IntPoly.Multiplication () 
import Numeric.AERN.Poly.IntPoly.Composition ()

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison
--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.RefinementOrderRounding (AbsEffortIndicator)

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder 
    (MinmaxEffortIndicator, MinmaxInOutEffortIndicator)
    -- needed for ghc 6.12
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder
    (JoinMeetEffortIndicator)
    -- needed for ghc 6.12
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Interval
--import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.SizeLimits
import Numeric.AERN.Basics.Consistency

import Test.QuickCheck (Arbitrary)

import Numeric.AERN.Misc.Debug
_ = unsafePrint

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     NumOrd.RefinementRoundedLatticeEffort (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    NumOrd.RoundedLatticeEffort (IntPoly var (Interval e))
    where
    type MinmaxEffortIndicator (IntPoly var (Interval e)) =
        IntPolyEffort (Interval e)
    minmaxDefaultEffort f@(IntPoly cfg _) =
        ipolycfg_effort cfg
        where
        maxdeg = ipolycfg_maxdeg cfg
        sampleDom = getSampleDomValue f

minmaxUpDnDefaultEffortIntPolyWithBezierDegree ::
    Int -> 
    (IntPoly var cf) -> 
    IntPolyEffort cf
minmaxUpDnDefaultEffortIntPolyWithBezierDegree degree _p@(IntPoly cfg _) =
    (ipolycfg_effort cfg)
    {
        ipolyeff_minmaxBernsteinDegreeMinus1 = Int1To10 (degree - 1)
    }

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     NumOrd.RefinementRoundedLattice (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    NumOrd.RoundedLattice (IntPoly var (Interval e))
    where
    maxUpEff eff a b =
--        unsafePrint 
--            ( "IntPoly maxUpEff:"
--                ++ "\n degreeMinusOne = " ++ show degreeMinusOne
--                ++ "\n cfg a = " ++ show (intpoly_cfg a)
--                ++ "\n cfg b = " ++ show (intpoly_cfg b)
--                ++ "\n a = " ++ showPoly show show a
--                ++ "\n b = " ++ showPoly show show b
--                ++ "\n a `maxUp` b = " ++ showPoly show show result 
--            ) $
        result
        where
        result =
            case (getConstantIfPolyConstant a, getConstantIfPolyConstant b) of
                (Just aC, Just bC) ->
                    newConstFnFromSample a $ NumOrd.maxOutEff effMinmaxDom aC bC 
                _ ->
                    fst $ maxUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (_aL,aR) = RefOrd.getEndpointsOutEff () a
        (_bL,bR) = RefOrd.getEndpointsOutEff () b
        
        effMinmax = 
            MinmaxEffortIndicatorFromRingOps -- TODO: insert efforts from eff instead of defaults
            {
                minmaxFromRO_eff_convertTDF = ArithUpDn.convertDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_roundedRealD = ArithInOut.roundedRealDefaultEffort sampleCf,
                minmaxFromRO_eff_getEndpointsD = RefOrd.getEndpointsDefaultEffort sampleCf,
                minmaxFromRO_eff_evalFT = evalOpsDefaultEffort sampleF sampleF,
                minmaxFromRO_eff_evalFDF = evalOpsDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_ringOpsF = ArithInOut.ringOpsDefaultEffort sampleF,
                minmaxFromRO_eff_mixmultFDF = ArithInOut.mixedMultDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_sizelimitsF = getSizeLimits sampleF,
                minmaxFromRO_eff_ringOpsT = ArithInOut.ringOpsDefaultEffort sampleF,
                minmaxFromRO_eff_mixedFldTDF = ArithInOut.mixedFieldOpsDefaultEffort sampleF sampleCf
            }
            where
            sampleCf = ipolycfg_sample_cf cfg
            (IntPoly cfg _) = a
            sampleF = a
        
        Int1To10 degreeMinusOne = ipolyeff_minmaxBernsteinDegreeMinus1 eff
        effMinmaxDom = ipolyeff_cfMinMaxEffort eff
        effDom = ipolyeff_cfRoundedRealEffort eff
        
        
--    maxDnEff (effMinmax, effMinmaxDom, Int1To10 degreeMinusOne, effGetE) a b =
--        result
--        where
--        result =
--            case (getConstantIfPolyConstant a, getConstantIfPolyConstant b) of
--                (Just aC, Just bC) ->
--                    newConstFnFromSample a $ NumOrd.maxOutEff effMinmaxDom aC bC 
--                _ ->
--                    maxDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
--        (aL,_aR) = RefOrd.getEndpointsOutEff effGetE a
--        (bL,_bR) = RefOrd.getEndpointsOutEff effGetE b
--    minUpEff (effMinmax, effMinmaxDom, Int1To10 degreeMinusOne,effGetE) a b = 
--        result
--        where
--        result =
--            case (getConstantIfPolyConstant a, getConstantIfPolyConstant b) of
--                (Just aC, Just bC) ->
--                    newConstFnFromSample a $ NumOrd.minOutEff effMinmaxDom aC bC 
--                _ ->
--                    minUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
--        (_aL,aR) = RefOrd.getEndpointsOutEff effGetE a
--        (_bL,bR) = RefOrd.getEndpointsOutEff effGetE b
--    minDnEff (effMinmax, effMinmaxDom, Int1To10 degreeMinusOne,effGetE) a b = 
--        result
--        where
--        result =
--            case (getConstantIfPolyConstant a, getConstantIfPolyConstant b) of
--                (Just aC, Just bC) ->
--                    newConstFnFromSample a $ NumOrd.minOutEff effMinmaxDom aC bC 
--                _ ->
--                    fst $ minDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
--        (aL,_aR) = RefOrd.getEndpointsOutEff effGetE a
--        (bL,_bR) = RefOrd.getEndpointsOutEff effGetE b

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    ArithUpDn.RoundedAbsEffort (IntPoly var (Interval e))
    where
    type AbsEffortIndicator (IntPoly var (Interval e)) =
        (NumOrd.MinmaxEffortIndicator (IntPoly var (Interval e)),
         ArithInOut.AbsEffortIndicator (Interval e))
    absDefaultEffort p =
        (NumOrd.minmaxDefaultEffort p,
         ArithInOut.absDefaultEffort $ getSampleDomValue p)

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    ArithUpDn.RoundedAbs (IntPoly var (Interval e))
    where
    absUpEff (effMinmax, effAbsDom) p =
        case getConstantIfPolyConstant p of
            Just c -> 
                newConstFnFromSample p $ 
                    snd $ RefOrd.getEndpointsOut $ 
                        ArithInOut.absOutEff effAbsDom c
            _ -> NumOrd.maxUpEff effMinmax p (neg p)
    absDnEff (effMinmax, effAbsDom) p =
        case getConstantIfPolyConstant p of
            Just c -> 
                newConstFnFromSample p $ 
                    fst $ RefOrd.getEndpointsOut $ 
                        ArithInOut.absOutEff effAbsDom c
            _ -> NumOrd.maxDnEff effMinmax p (neg p)


instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    NumOrd.RefinementRoundedLatticeEffort (IntPoly var (Interval e))
    where
    type MinmaxInOutEffortIndicator (IntPoly var (Interval e)) =
        IntPolyEffort (Interval e)

    minmaxInOutDefaultEffort f@(IntPoly cfg _) =
        ipolycfg_effort cfg

minmaxInOutDefaultEffortIntPolyWithBezierDegree degree _p@(IntPoly cfg _) =
    (ipolycfg_effort cfg)
    {
        ipolyeff_minmaxBernsteinDegreeMinus1 = Int1To10 (degree - 1)
    }

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    NumOrd.RefinementRoundedLattice (IntPoly var (Interval e))
    where
    maxOutEff eff a b =
        result
        where
        result =
            makeCoeffsConsistentOut $ 
            RefOrd.fromEndpointsOutEff () (resL,resR)
        resL = maxDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        (resR, _) = maxUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (aL,aR) = RefOrd.getEndpointsOutEff () a
        (bL,bR) = RefOrd.getEndpointsOutEff () b

        effMinmax = 
            MinmaxEffortIndicatorFromRingOps -- TODO: insert efforts from eff instead of defaults
            {
                minmaxFromRO_eff_convertTDF = ArithUpDn.convertDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_roundedRealD = ArithInOut.roundedRealDefaultEffort sampleCf,
                minmaxFromRO_eff_getEndpointsD = RefOrd.getEndpointsDefaultEffort sampleCf,
                minmaxFromRO_eff_evalFT = evalOpsDefaultEffort sampleF sampleF,
                minmaxFromRO_eff_evalFDF = evalOpsDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_ringOpsF = ArithInOut.ringOpsDefaultEffort sampleF,
                minmaxFromRO_eff_mixmultFDF = ArithInOut.mixedMultDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_sizelimitsF = getSizeLimits sampleF,
                minmaxFromRO_eff_ringOpsT = ArithInOut.ringOpsDefaultEffort sampleF,
                minmaxFromRO_eff_mixedFldTDF = ArithInOut.mixedFieldOpsDefaultEffort sampleF sampleCf
            }
            where
            sampleCf = ipolycfg_sample_cf cfg
            (IntPoly cfg _) = a
            sampleF = a
        
        Int1To10 degreeMinusOne = ipolyeff_minmaxBernsteinDegreeMinus1 eff

    maxInEff =
        error "aern-poly: inner-rounded max not available for IntPoly"
    minOutEff eff a b =
        result
        where
        result = 
            makeCoeffsConsistentOut $ 
            RefOrd.fromEndpointsOutEff () (resL,resR)
        (resL, _) = minDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        resR = minUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (aL,aR) = RefOrd.getEndpointsOutEff () a
        (bL,bR) = RefOrd.getEndpointsOutEff () b

        effMinmax = 
            MinmaxEffortIndicatorFromRingOps -- TODO: insert efforts from eff instead of defaults
            {
                minmaxFromRO_eff_convertTDF = ArithUpDn.convertDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_roundedRealD = ArithInOut.roundedRealDefaultEffort sampleCf,
                minmaxFromRO_eff_getEndpointsD = RefOrd.getEndpointsDefaultEffort sampleCf,
                minmaxFromRO_eff_evalFT = evalOpsDefaultEffort sampleF sampleF,
                minmaxFromRO_eff_evalFDF = evalOpsDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_ringOpsF = ArithInOut.ringOpsDefaultEffort sampleF,
                minmaxFromRO_eff_mixmultFDF = ArithInOut.mixedMultDefaultEffort sampleF sampleCf,
                minmaxFromRO_eff_sizelimitsF = getSizeLimits sampleF,
                minmaxFromRO_eff_ringOpsT = ArithInOut.ringOpsDefaultEffort sampleF,
                minmaxFromRO_eff_mixedFldTDF = ArithInOut.mixedFieldOpsDefaultEffort sampleF sampleCf
            }
            where
            sampleCf = ipolycfg_sample_cf cfg
            (IntPoly cfg _) = a
            sampleF = a
        
        Int1To10 degreeMinusOne = ipolyeff_minmaxBernsteinDegreeMinus1 eff

    minInEff =
        error "aern-poly: inner-rounded min not available for IntPoly"
    
    
instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    ArithInOut.RoundedAbsEffort (IntPoly var (Interval e))
    where
    type AbsEffortIndicator (IntPoly var (Interval e)) =
        (NumOrd.MinmaxInOutEffortIndicator (IntPoly var (Interval e)),
         ArithInOut.AbsEffortIndicator (Interval e))
    absDefaultEffort p =
        (NumOrd.minmaxInOutDefaultEffort p,
         ArithInOut.absDefaultEffort $ getSampleDomValue p)

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency (Interval e), 
     Arbitrary (Interval e), 
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithUpDn.Convertible (Interval e) (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     Show var,
     Show (Interval e),
     Show (Imprecision (Interval e)),
     NumOrd.PartialComparison (Imprecision (Interval e)))
    =>
    ArithInOut.RoundedAbs (IntPoly var (Interval e))
    where
    absOutEff (effMinmax, effAbsDom) p =
        case getConstantIfPolyConstant p of
            Just c -> newConstFnFromSample p $ ArithInOut.absOutEff effAbsDom c
            _ -> NumOrd.maxOutEff effMinmax p (neg p)
    absInEff =
        error "aern-poly: inner-rounded abs not available for IntPoly"
    
instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency cf, 
     Arbitrary cf, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf cf,
     Show var,
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    RefOrd.RoundedLatticeEffort (IntPoly var cf)
    where
    type JoinMeetEffortIndicator (IntPoly var cf) =
        RefOrd.JoinMeetEffortIndicator cf

    joinmeetDefaultEffort (IntPoly cfg _) =
        RefOrd.joinmeetDefaultEffort $ ipolycfg_sample_cf cfg

--joinmeetDefaultEffortIntPolyWithBezierDegree degree f =
--    (defaultMinmaxEffortIndicatorFromRingOps f f,
--     Int1To10 (degree - 1),
--     RefOrd.getEndpointsDefaultEffort f,
--     RefOrd.fromEndpointsDefaultEffort f)

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency cf, 
     Arbitrary cf, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf cf,
     Show var,
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    RefOrd.RoundedLattice (IntPoly var cf)
    where
    joinOutEff =
        error "aern-poly: join not defined for IntPoly"
    joinInEff =
        error "aern-poly: join not defined for IntPoly"
    meetOutEff eff a b@(IntPoly cfg _) =
        polyJoinWith (zero sampleCf) (uncurry $ RefOrd.meetOutEff eff) (a, b)
        where
        sampleCf = ipolycfg_sample_cf cfg
    meetInEff =
        error "aern-poly: inner-rounded meet not defined for IntPoly"

{-|
    Take a sample polynomial with at least one variable in its domain
    and return the univariate polynomial \x : [0,1] -> x.  
-}
getX ::
    (Ord var, Show var, 
     Show cf, HasConsistency cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    =>
    (IntPoly var cf) -> (IntPoly var cf)
getX (IntPoly (IntPolyCfg vars _ _ sample limits _) _) =
    newProjection limits [(var, unit)] var
    where
    var = head vars
    unit = RefOrd.fromEndpointsOut (zero sample, one sample)
    
getDegree :: Int -> IntPoly var cf -> Int
getDegree degreeMinusOne (IntPoly cfg _) =
    max 2 $ min (degreeMinusOne + 1) maxDeg
    where
    maxDeg = ipolycfg_maxdeg cfg