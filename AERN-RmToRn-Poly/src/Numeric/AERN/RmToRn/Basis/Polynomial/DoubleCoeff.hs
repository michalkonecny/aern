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
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show, 
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain,
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.New
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly (PolyFP)
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain()
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.New()

