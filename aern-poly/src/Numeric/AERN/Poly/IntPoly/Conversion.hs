{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Conversion
    Description :  conversions to and from common numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions to and from common numeric types.
-}

module Numeric.AERN.Poly.IntPoly.Conversion
--    (
--    )
where

import           Numeric.AERN.Poly.IntPoly.Config
import           Numeric.AERN.Poly.IntPoly.Evaluation ()
import           Numeric.AERN.Poly.IntPoly.IntPoly
--import           Numeric.AERN.Poly.IntPoly.New
--import           Numeric.AERN.Poly.IntPoly.Show
import           Numeric.AERN.RmToRn.Evaluation

import           Numeric.AERN.RmToRn.Domain
import           Numeric.AERN.RmToRn.New

import           Numeric.AERN.RealArithmetic.NumericOrderRounding                    
                                                                                       (ConvertEffortIndicator)
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding                     as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding                  as ArithInOut
--import           Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

import           Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder                                         as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import           Numeric.AERN.Basics.Interval
import           Numeric.AERN.Basics.SizeLimits
import           Numeric.AERN.Basics.Consistency

--import qualified Data.IntMap                                                          as IntMap
--import           Data.List                                                           (elemIndex)
--import qualified Data.Map                                                             as Map

{-- Basic function-approximation specific ops --}

instance
    (HasSampleFromContext cf,
     HasSampleFromContext var,
     Ord var, Show var, Show cf, 
     RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf)
    =>
    HasSampleFromContext (IntPoly var cf)
    where
    sampleFromContext = 
        newConstFn sizeLimits [(sampleFromContext, sampleCf)] sampleCf
        where
        sizeLimits = defaultIntPolySizeLimits sampleCf (defaultSizeLimits sampleCf) arity 
        sampleCf = sampleFromContext
        arity = 1

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     RefOrd.IntervalLike cf)
    =>
    HasZero (IntPoly var cf)
    where
    zero sampleP = newConstFnFromSample sampleP $ zero sampleCf
        where
        sampleCf = getSampleDomValue sampleP

instance
    (Ord var, Show var, Show cf,
     HasConsistency cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    HasOne (IntPoly var cf)
    where
    one sampleP = newConstFnFromSample sampleP $ one sampleCf
        where
        sampleCf = getSampleDomValue sampleP

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     RefOrd.IntervalLike cf)
    =>
    HasInfinities (IntPoly var cf)
    where
    plusInfinity sampleP = newConstFnFromSample sampleP $ plusInfinity sampleCf
        where
        sampleCf = getSampleDomValue sampleP
    minusInfinity sampleP = newConstFnFromSample sampleP $ minusInfinity sampleCf
        where
        sampleCf = getSampleDomValue sampleP
    excludesMinusInfinity (IntPoly _cfg terms) =
        and $ termsCollectCoeffsWith excludesInfty terms
        where
        excludesInfty _ coeff =
            excludesMinusInfinity coeff 
    excludesPlusInfinity (IntPoly _cfg terms) =
        and $ termsCollectCoeffsWith excludesInfty terms
        where
        excludesInfty _ coeff =
            excludesPlusInfinity coeff 

convertToDefaultEffortStandard :: 
    IntPoly var cf -> 
    t -> 
    IntPolyEffort cf
convertToDefaultEffortStandard _sampleP@(IntPoly cfg _) _sampleT =
    ipolycfg_effort cfg

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible cf Integer,
     Show cf,  Show (SizeLimits cf))
    =>
    ArithUpDn.Convertible (IntPoly var cf) Integer
    where
    type ConvertEffortIndicator (IntPoly var cf) Integer =
        IntPolyEffort cf
    convertDefaultEffort = convertToDefaultEffortStandard
    convertUpEff = convertUpEffStandard ArithInOut.rrEffortToInteger 
    convertDnEff = convertDnEffStandard ArithInOut.rrEffortToInteger


instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible cf Int,
     Show cf,  Show (SizeLimits cf))
    =>
    ArithUpDn.Convertible (IntPoly var cf) Int
    where
    type ConvertEffortIndicator (IntPoly var cf) Int =
        IntPolyEffort cf
    convertDefaultEffort = convertToDefaultEffortStandard
    convertUpEff = convertUpEffStandard ArithInOut.rrEffortToInt 
    convertDnEff = convertDnEffStandard ArithInOut.rrEffortToInt

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible cf Rational,
     Show cf,  Show (SizeLimits cf))
    =>
    ArithUpDn.Convertible (IntPoly var cf) Rational
    where
    type ConvertEffortIndicator (IntPoly var cf) Rational =
        IntPolyEffort cf
    convertDefaultEffort = convertToDefaultEffortStandard
    convertUpEff = convertUpEffStandard ArithInOut.rrEffortToRational 
    convertDnEff = convertDnEffStandard ArithInOut.rrEffortToRational

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible cf Double,
     Show cf,  Show (SizeLimits cf))
    =>
    ArithUpDn.Convertible (IntPoly var cf) Double
    where
    type ConvertEffortIndicator (IntPoly var cf) Double =
        IntPolyEffort cf
    convertDefaultEffort = convertToDefaultEffortStandard
    convertUpEff = convertUpEffStandard ArithInOut.rrEffortToDouble 
    convertDnEff = convertDnEffStandard ArithInOut.rrEffortToDouble

instance
    (Ord var, Show var,
     cf ~ Interval e,
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     Show cf,  Show (SizeLimits cf))
    =>
    ArithUpDn.Convertible (IntPoly var (Interval e)) (Interval e)
    where
    type ConvertEffortIndicator (IntPoly var (Interval e)) (Interval e) =
        (IntPolyEffort (Interval e))
    convertDefaultEffort sampleP@(IntPoly _cfg _) sampleI = 
        (convertToDefaultEffortStandard sampleP sampleI)
        where
--        sampleCf = ipolycfg_sample_cf cfg
    convertUpEff eff = convertUpEffStandard (\ _ _ -> ()) eff
    convertDnEff eff = convertDnEffStandard (\ _ _ -> ()) eff
--    convertUpEff (eff, effConv) = convertUpEffStandard (\ _ _ -> effConv) eff 
--    convertDnEff (eff, effConv) = convertDnEffStandard (\ _ _ -> effConv) eff 


convertUpEffStandard, convertDnEffStandard ::
      (Ord var, Show cf, Show var, RefOrd.IntervalLike cf,
       ArithUpDn.Convertible cf t, ArithInOut.RoundedReal cf,
       HasConsistency cf) 
       =>
      (cf -> ArithInOut.RoundedRealEffortIndicator cf -> ConvertEffortIndicator cf t) -> 
      IntPolyEffort cf -> t -> 
      IntPoly var cf -> 
      Maybe t

convertUpEffStandard rrEffortToT eff sampleT p =
    convertUpEffGeneric (effStandardToGeneric sampleT rrEffortToT eff p) sampleT p

convertDnEffStandard rrEffortToT eff sampleT p =
    convertDnEffGeneric (effStandardToGeneric sampleT rrEffortToT eff p) sampleT p
    
effStandardToGeneric :: 
      t 
      -> (cf -> ArithInOut.RoundedRealEffortIndicator cf -> (ConvertEffortIndicator cf t))
      -> IntPolyEffort cf
      -> (IntPoly var cf)
      -> ConvertEffortIndicatorGeneric var cf t
effStandardToGeneric _sampleT rrEffortToT eff _p@(IntPoly cfg _) =
    (effEval, effGetEndpts, effConv)
    where
    effEval = eff
    effGetEndpts = ipolyeff_cfGetEndpointsEffort eff
    effConv = rrEffortToT sampleCf effCf
    effCf = ipolyeff_cfRoundedRealEffort eff
    sampleCf = ipolycfg_sample_cf cfg
    
convertUpEffGeneric, convertDnEffGeneric :: 
      (Show (Domain f), RefOrd.IntervalLike (Domain f),
       ArithUpDn.Convertible (Domain f) t, HasEvalOps f (Domain f)) 
       =>
      (EvalOpsEffortIndicator f (Domain f),
       RefOrd.GetEndpointsEffortIndicator (Domain f),
       ConvertEffortIndicator (Domain f) t)
      -> 
      t -> 
      f -> 
      Maybe t
convertUpEffGeneric (effEval, effGetEndpts, effConv) sampleT p =
    ArithUpDn.convertUpEff effConv sampleT $ 
        snd $ RefOrd.getEndpointsOutEff effGetEndpts range
    where
    range = evalOtherType (evalOpsEff effEval sampleP sampleCf) varDoms p
    varDoms = getDomainBox p
    sampleP = p
    sampleCf = getSampleDomValue sampleP
convertDnEffGeneric (effEval, effGetEndpts, effConv) sampleT p =
    ArithUpDn.convertDnEff effConv sampleT $ 
        fst $ RefOrd.getEndpointsOutEff effGetEndpts range
    where
    range = evalOtherType (evalOpsEff effEval sampleP sampleCf) varDoms p
    varDoms = getDomainBox p
    sampleP = p
    sampleCf = getSampleDomValue sampleP

type ConvertEffortIndicatorGeneric var cf t =
    (EvaluationEffortIndicator (IntPoly var cf),
     RefOrd.GetEndpointsEffortIndicator cf,
     ArithUpDn.ConvertEffortIndicator cf t)

{---- Conversions FROM type t to IntPoly  ----}     
     
convertFromDefaultEffortStandard :: 
    t -> 
    IntPoly var cf -> 
    IntPolyEffort cf
convertFromDefaultEffortStandard  _sampleT _sampleP@(IntPoly cfg _) =
    ipolycfg_effort cfg
    
instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible Int cf,
     Show cf)
    =>
    ArithUpDn.Convertible Int (IntPoly var cf)
    where
    type ConvertEffortIndicator Int (IntPoly var cf) =
        IntPolyEffort cf
    convertDefaultEffort = convertFromDefaultEffortStandard
    convertUpEff = convertUpDnEffStandardFrom snd ArithInOut.rrEffortFromInt
    convertDnEff = convertUpDnEffStandardFrom fst ArithInOut.rrEffortFromInt

convertUpDnEffStandardFrom :: 
      (Ord var, Show var, Show cf, ArithInOut.RoundedReal cf,
       RefOrd.IntervalLike cf, HasConsistency cf,
       ArithInOut.Convertible t cf) 
      =>
      ((cf,cf) -> cf)
      ->
      (cf
       -> ArithInOut.RoundedRealEffortIndicator cf
       -> ArithInOut.ConvertEffortIndicator t cf
      )
      -> IntPolyEffort cf 
      -> IntPoly var cf 
      -> t 
      -> Maybe (IntPoly var cf)
convertUpDnEffStandardFrom pick rrEffortFrom eff sampleP@(IntPoly cfg _) n =
    Just $
        newConstFnFromSample sampleP $
            pick $ RefOrd.getEndpointsOutEff effGetEndpts $
                ArithInOut.convertOutEff effConv sampleCf n
    where
    effGetEndpts = ipolyeff_cfGetEndpointsEffort eff
    effConv = rrEffortFrom sampleCf effCf
    effCf = ipolyeff_cfRoundedRealEffort eff
    sampleCf = ipolycfg_sample_cf cfg

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible Integer cf,
     Show cf)
    =>
    ArithUpDn.Convertible Integer (IntPoly var cf)
    where
    type ConvertEffortIndicator Integer (IntPoly var cf) =
        IntPolyEffort cf
    convertDefaultEffort = convertFromDefaultEffortStandard
    convertUpEff = convertUpDnEffStandardFrom snd ArithInOut.rrEffortFromInteger
    convertDnEff = convertUpDnEffStandardFrom fst ArithInOut.rrEffortFromInteger


instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible Rational cf,
     Show cf)
    =>
    ArithUpDn.Convertible Rational (IntPoly var cf)
    where
    type ConvertEffortIndicator Rational (IntPoly var cf) =
        IntPolyEffort cf
    convertDefaultEffort = convertFromDefaultEffortStandard
    convertUpEff = convertUpDnEffStandardFrom snd ArithInOut.rrEffortFromRational
    convertDnEff = convertUpDnEffStandardFrom fst ArithInOut.rrEffortFromRational


instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible Double cf,
     Show cf)
    =>
    ArithUpDn.Convertible Double (IntPoly var cf)
    where
    type ConvertEffortIndicator Double (IntPoly var cf) =
        IntPolyEffort cf
    convertDefaultEffort = convertFromDefaultEffortStandard
    convertUpEff = convertUpDnEffStandardFrom snd ArithInOut.rrEffortFromDouble
    convertDnEff = convertUpDnEffStandardFrom fst ArithInOut.rrEffortFromDouble
