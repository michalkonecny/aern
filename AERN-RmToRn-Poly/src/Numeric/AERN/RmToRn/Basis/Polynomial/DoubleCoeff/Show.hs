{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
    showPolyFPWithVars,
    showPolyFPChebTermsWithVars
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
    show = showUsingShowInternals

instance
        (ShowInternals (PolyFP))
    where
    type ShowInternalsIndicator (PolyFP) = 
        (Bool, ShowInternalsIndicator Double)
    defaultShowIndicator _ = (False, False)
    showInternals (showChebyshevTerms, coeffShowIndicator) polyFP =
        showPolyFPWithVars polyFP coeffShowIndicator varNamesArity
        ++
        (case showChebyshevTerms of
            True -> 
                " [" ++ showPolyFPChebTermsWithVars polyFP coeffShowIndicator varNamesArity ++ "]"
            False -> "")
        where
        varNamesArity = varNames (fromIntegral maxArity)
        (Var maxArity) = peekArity polyFP
        varNames arity = 
            take arity $ 
                ["x", "y", "z"] 
                ++ (map (\n -> "v" ++ show n ) [3..(arity - 1)])
        
showPolyFPWithVars :: 
    PolyFP -> (ShowInternalsIndicator Double) -> [HVar] -> String
showPolyFPWithVars polyFP coeffShowIndicator varNames =
    showSymbolicPoly (showInternals coeffShowIndicator) defaultShowVar $
            evalAtPtChebBasis 
                polyFP 
                (map hpolyVar varNames) 
                hpolyOne
                (hpolyAdd (<+>))
                (hpolySubtr neg (<+>))
                (hpolyMult (<+>) (<*>))
                (\coeff -> hpolyConst $ Interval coeff coeff)
        
showPolyFPChebTermsWithVars :: 
    PolyFP -> (ShowInternalsIndicator Double) -> [HVar] -> String
showPolyFPChebTermsWithVars polyFP coeffShowIndicator varNames =
    showSymbolicPoly (showInternals coeffShowIndicator) chebyShowVar $
            evalAtPtPowerBasis
            -- trick: treat the polynomial as if it was in power basis
            --        thus obtaining the raw coefficients of the Chebyshev terms
                polyFP 
                (map hpolyVar varNames) 
                hpolyOne
                (hpolyAdd (<+>))
                (hpolyMult (<+>) (<*>))
                (\coeff -> hpolyConst $ Interval coeff coeff)

chebyShowVar var power = 
    "T_" ++ show power ++ "(" ++ var ++ ")"
