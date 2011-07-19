{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly
    Description :  arithmetic of Polynomials with Haskell coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff 
(
    -- TODO: export only PolyPure and PolyFP as abstract data types
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.RingOps,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.New
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.New(PolySizeLimits(..))
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Conversion()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Evaluation()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.RingOps()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show

import Numeric.AERN.RmToRn.MinimalFnBasis
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.ShowInternals
import Foreign.Storable

import Test.QuickCheck

instance
    (ArithUpDn.RoundedRealInPlace cf, 
     Storable cf, 
     ShowInternals cf, Show cf, 
     Arbitrary cf, NumOrd.ArbitraryOrderedTuple cf)
    =>
    MinimalFnBasis (Poly cf)
    where
    fixedDomain _ = (-1) </\> 1
