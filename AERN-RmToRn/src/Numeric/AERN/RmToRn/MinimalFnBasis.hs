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
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.ShowInternals

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

class (HasDomainBox fb,
--        CanEvaluate fb, -- value at a point
--        CanSubstitute fb, -- substitution
        CanEvaluateOtherType fb, -- another interpretation (eg string or interval)
        ShowInternals fb,
        HasProjections fb, -- variables (but their domain is fixed!)
        HasConstFns fb, -- constants
        ArithUpDn.Convertible fb (Domain fb), -- bounds
        CanBeMutable fb,
        -- ring ops rounded up/down:
        ArithUpDn.RoundedAddInPlace fb,
        ArithUpDn.RoundedMultiplyInPlace fb,
        ArithUpDn.RoundedMixedAddInPlace fb (Domain fb),
        ArithUpDn.RoundedMixedMultiplyInPlace fb (Domain fb),
        -- ring ops rounded *out* by some internal notion of enclosure:
        ArithInOut.RoundedAddInPlace fb,
        ArithInOut.RoundedMultiplyInPlace fb,
        ArithInOut.RoundedMixedAddInPlace fb (Domain fb),
        ArithInOut.RoundedMixedMultiplyInPlace fb (Domain fb)
      ) => 
    MinimalFnBasis fb
    where
    fixedDomain :: fb -> Interval (Domain fb)
    
