{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly
    Description :  arithmetic of Polynomials with generic coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with generic coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff 
(
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.FieldOps,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate,
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.FieldOps
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.New()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Conversion()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Evaluation()
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show

