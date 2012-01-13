{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Plot
    Description :  Cairo plotting support
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Cairo plotting support.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Plot
(
)
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation

import Numeric.AERN.RmToRn.Plot.CairoDrawable
import Numeric.AERN.RmToRn.Plot.FromEval

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

instance
    (Show cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedReal cf,
     Ord var,
     Show var,
     HasAntiConsistency cf)
    =>
    CairoDrawableFn (IntPoly var cf)
    where
    type CairoDrawFnEffortIndicator (IntPoly var cf) =
        CairoDrawEffortIndicatorFnFromEval (IntPoly var cf)
    cairoDrawFnDefaultEffort =
        cairoDrawFnDefaultEffortFromEval
    cairoDrawFn = 
        cairoDrawFnFromEval 
