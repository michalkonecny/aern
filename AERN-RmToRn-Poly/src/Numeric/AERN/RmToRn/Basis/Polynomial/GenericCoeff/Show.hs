{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show
    Description :  string representation of Poly
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    String representation of Poly with generic coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show 
(
    showPolyFPWithVars
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Symbolic

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval()

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

--import Numeric.AERN.Misc.Debug

import Foreign.Storable

instance 
        (ShowInternals cf, Storable cf,
         ArithUpDn.RoundedReal cf) => 
        (Show (PolyFP cf)) 
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
    (ShowInternals cf, 
     Storable cf,
     ArithUpDn.RoundedReal cf) => 
    PolyFP cf -> [HVar] -> String
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
