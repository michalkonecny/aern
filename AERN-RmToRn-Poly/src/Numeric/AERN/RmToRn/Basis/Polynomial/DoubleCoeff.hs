{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly
    Description :  arithmetic of Polynomials with Double coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with native Double coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff 
(
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain()
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.New()
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Conversion()
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show

