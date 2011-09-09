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
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps,
        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate
--    ,
--        module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder
    )
where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps ()
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate --()
--import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder --()
