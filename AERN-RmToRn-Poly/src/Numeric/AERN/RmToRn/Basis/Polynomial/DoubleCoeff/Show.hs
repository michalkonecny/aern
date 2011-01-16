{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show
    Description :  string representation of Poly
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    String representation of Poly with native Double coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show 
(
    showPolyFPWithVars
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Symbolic

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval()

import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.Basis.Double()
import Numeric.AERN.RealArithmetic.Interval.Double()

--import Numeric.AERN.Misc.Debug

instance 
        (Show (PolyFP)) 
    where
    show polyFP =
        showPolyFPWithVars polyFP $ varNames (fromIntegral maxArity)
        where
        (Var maxArity) = peekArity polyFP
        varNames arity = 
            take arity $ 
                ["x", "y", "z"] 
                ++ (map (\n -> "v" ++ show n ) [3..(arity - 1)])
        
showPolyFPWithVars :: 
    PolyFP -> [HVar] -> String
showPolyFPWithVars polyFP varNames =
    show $
            evalAtPtChebBasis 
                polyFP 
                (map hpolyVar varNames) 
                hpolyOne
                (hpolyAdd (<+>))
                (hpolySubtr neg (<+>))
                (hpolyMult (<+>) (<*>))
                (\coeff -> hpolyConst $ Interval coeff coeff)

