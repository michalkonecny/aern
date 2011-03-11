{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.MinimalFnBasis
    Description :  class specifying a core function basis on a fixed domain  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Class specifying a core function basis on a fixed domain.
    
    This module should be only used by libraries that provide
    an alternative function basis.
-}

module Numeric.AERN.RmToRn.MinimalFnBasis where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluate

import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
--import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

class (HasDomainBox fb,
--        CanEvaluate fb, -- value at a point
--        CanSubstitute fb, -- substitution
        CanEvaluateOtherType fb, -- another interpretation (eg string or interval)
        HasProjections fb, -- variables (but their domain is fixed!)
        HasConstFns fb, -- constants
        ArithUpDn.Convertible (Domain fb) fb, -- constants
        ArithUpDn.Convertible fb (Domain fb), -- bounds
        ArithUpDn.RoundedAdd fb,
        ArithUpDn.RoundedMultiply fb,
        ArithUpDn.RoundedMixedAdd fb (Domain fb),
        ArithUpDn.RoundedMixedMultiply fb (Domain fb)
      ) => 
    MinimalFnBasis fb
    where
    fixedDomain :: fb -> Interval (Domain fb)
    
