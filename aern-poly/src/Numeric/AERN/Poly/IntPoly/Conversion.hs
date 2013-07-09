{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts          #-}
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
import           Numeric.AERN.Poly.IntPoly.Evaluation
import           Numeric.AERN.Poly.IntPoly.IntPoly
import           Numeric.AERN.Poly.IntPoly.New
import           Numeric.AERN.Poly.IntPoly.Show
import           Numeric.AERN.RmToRn.Evaluation

import           Numeric.AERN.RmToRn.Domain
import           Numeric.AERN.RmToRn.New

import           Numeric.AERN.RealArithmetic.NumericOrderRounding                    
                                                                                       (ConvertEffortIndicator)
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding                     as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding                  as ArithInOut
import           Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

import           Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder                                         as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import           Numeric.AERN.Basics.SizeLimits
import           Numeric.AERN.Basics.Consistency

import qualified Data.IntMap                                                          as IntMap
import           Data.List                                                           
                                                                                       (elemIndex)
import qualified Data.Map                                                             as Map

{-- Basic function-approximation specific ops --}


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

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf,
     ArithUpDn.Convertible cf t,
     Show cf,  Show (SizeLimits cf))
    =>
    ArithUpDn.Convertible (IntPoly var cf) t
    where
    type ConvertEffortIndicator (IntPoly var cf) t =
        (EvaluationEffortIndicator (IntPoly var cf),
         RefOrd.GetEndpointsEffortIndicator cf,
         ArithUpDn.ConvertEffortIndicator cf t)
    convertDefaultEffort sampleP sampleT =
        (evaluationDefaultEffort sampleP,
         RefOrd.getEndpointsDefaultEffort sampleCf,
         ArithUpDn.convertDefaultEffort sampleCf sampleT)
        where
        sampleCf = getSampleDomValue sampleP
    convertUpEff (effEval, effGetEndpts, effConv) sampleT p =
        ArithUpDn.convertUpEff effConv sampleT $ 
            snd $ RefOrd.getEndpointsOutEff effGetEndpts range
        where
        range = evalOtherType (evalOpsEff effEval sampleP sampleCf) varDoms p
        varDoms = getDomainBox p
        sampleP = p
        sampleCf = getSampleDomValue sampleP
    convertDnEff (effEval, effGetEndpts, effConv) sampleT p =
        ArithUpDn.convertDnEff effConv sampleT $ 
            fst $ RefOrd.getEndpointsOutEff effGetEndpts range
        where
        range = evalOtherType (evalOpsEff effEval sampleP sampleCf) varDoms p
        varDoms = getDomainBox p
        sampleP = p
        sampleCf = getSampleDomValue sampleP

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
        (ArithInOut.ConvertEffortIndicator Int cf,
         RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort sampleI sampleP =
        (ArithInOut.convertDefaultEffort sampleI sampleCf,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    convertUpEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                snd $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP
    convertDnEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                fst $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP

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
        (ArithInOut.ConvertEffortIndicator Integer cf,
         RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort sampleI sampleP =
        (ArithInOut.convertDefaultEffort sampleI sampleCf,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    convertUpEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                snd $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP
    convertDnEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                fst $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP


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
        (ArithInOut.ConvertEffortIndicator Rational cf,
         RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort sampleI sampleP =
        (ArithInOut.convertDefaultEffort sampleI sampleCf,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    convertUpEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                snd $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP
    convertDnEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                fst $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP


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
        (ArithInOut.ConvertEffortIndicator Double cf,
         RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort sampleI sampleP =
        (ArithInOut.convertDefaultEffort sampleI sampleCf,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    convertUpEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                snd $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP
    convertDnEff (effConv, effGetEndpts) sampleP n =
        Just $
            newConstFnFromSample sampleP $
                fst $ RefOrd.getEndpointsOutEff effGetEndpts $
                    ArithInOut.convertOutEff effConv sampleCf n 
        where
        sampleCf = getSampleDomValue sampleP
