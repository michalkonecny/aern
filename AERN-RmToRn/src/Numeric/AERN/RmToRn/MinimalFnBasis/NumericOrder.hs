{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.MinimalFnBasis.NumericOrder
    Description :  comparing fucntions and generating random ones
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Comparing fucntions and generating random ones.
-}

module Numeric.AERN.RmToRn.MinimalFnBasis.NumericOrder where

import Numeric.AERN.RmToRn.MinimalFnBasis.Basics

import Numeric.AERN.RmToRn.Domain

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.Interval (Interval(..))
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Misc.Debug

instance (MinimalFnBasis fb
    , Show fb, Show (Domain fb)
    ) 
    => NumOrd.PartialComparison (FnEndpoint fb)
    where
    type NumOrd.PartialCompareEffortIndicator (FnEndpoint fb) =
        (ArithUpDn.AddEffortIndicator fb, 
         ArithUpDn.ConvertEffortIndicator fb (Domain fb),
         NumOrd.PartialCompareEffortIndicator (Domain fb))
    pCompareDefaultEffort (FnEndpoint f) =
        (ArithUpDn.addDefaultEffort f,
         ArithUpDn.convertDefaultEffort f (getSampleDomValue f),
         NumOrd.pCompareDefaultEffort (getSampleDomValue f))
    pCompareEff (effAdd, effBnd, effCompDom) f1 f2 =
--        unsafePrint
--        (
--            "FnEndpoint NumOrd.pCompareEff:"
--            ++ "\n f1 = " ++ show f1
--            ++ "\n f2 = " ++ show f2
--            ++ "\n diffUp = " ++ show diffUp
--            ++ "\n diffDn = " ++ show diffDn
--            ++ "\n bUp = " ++ show bUp
--            ++ "\n bDn = " ++ show bDn
--        ) $
        NumOrd.pCompareEff effCompDom (Interval bDn bUp) zero
        where
        _ = [bUp, getSampleDomValue f1]
        Just bUp = ArithUpDn.convertUpEff effBnd diffUp
        Just bDn = ArithUpDn.convertDnEff effBnd diffDn
        diffUp = ArithUpDn.subtrUpEff effAdd f1 f2 
        diffDn = ArithUpDn.subtrDnEff effAdd f1 f2 
        
--instance (MinimalFnBasis fb) => NumOrd.ArbitraryOrderedTuple (FnEndpoint fb)
        