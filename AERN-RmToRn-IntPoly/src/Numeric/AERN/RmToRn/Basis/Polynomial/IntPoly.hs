{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly
    Description :  datatype of polynomials with interval coefficients  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Datatype of polynomials with interval coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly 
    (
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Integrate,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder
    )
where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps (sinePoly)
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Integrate
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder ()


import Numeric.AERN.RmToRn.MinimalFnBasis

--import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.Basics.Exception

import qualified Data.Map as Map

--instance 
--    (ArithInOut.RoundedReal cf, HasLegalValues cf, 
--     Show cf, Ord var, Show var) 
--    => MinimalFnBasis (IntPoly var cf)
--    where
--    fitDomainBoxToSizeLimits _ cfg _ = 
--        Map.fromAscList $ zip vars doms
--        where
--        vars = ipolycfg_vars cfg
--        doms = ipolycfg_doms cfg
    