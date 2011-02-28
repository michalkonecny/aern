{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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

--import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import Numeric.AERN.Misc.Debug

import Foreign.Storable

instance 
        (ShowInternals cf, Storable cf,
         ArithUpDn.RoundedReal cf) => 
        (Show (PolyFP cf)) 
    where
    show = showUsingShowInternals

instance
        (ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
        (ShowInternals (PolyFP cf))
    where
    type ShowInternalsIndicator (PolyFP cf) = 
        (Bool, ShowInternalsIndicator cf)
    defaultShowIndicator p = (False, defaultShowIndicator $ peekConst p)
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
    (ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
    PolyFP cf -> (ShowInternalsIndicator cf) -> [HVar] -> String
showPolyFPWithVars poly coeffShowIndicator varNames 
    | errorBoundKnownToBeZero = polyText
    | otherwise = polyText ++ errorText
    where
    errorBoundKnownToBeZero =
        case errorBound ==? zero of Just True -> True; _ -> False
    errorText = " ± " ++ (showInternals coeffShowIndicator errorBound)
    errorBound = peekError poly
    polyText = 
        showSymbolicPoly (showInternals coeffShowIndicator) defaultShowVar $
                evalAtPtChebBasis 
                    poly
                    (map hpolyVar varNames) 
                    hpolyOne
                    (hpolyAdd (<+>))
                    (hpolySubtr neg (<+>))
                    (hpolyMult (<+>) (<*>))
                    (\coeff -> hpolyConst $ Interval coeff coeff)
        
showPolyFPChebTermsWithVars :: 
    (ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
    PolyFP cf -> (ShowInternalsIndicator cf) -> [HVar] -> String
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

