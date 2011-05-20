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
    showPolyWithVars
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate
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

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST, runST)

import Foreign.Storable

instance 
        (ShowInternals cf, Storable cf,
         CanBeMutable cf, Show cf, 
         ArithUpDn.RoundedRealInPlace cf) => 
        (Show (Poly cf)) 
    where
    show = showUsingShowInternals

instance
        (ShowInternals cf, Storable cf,
         CanBeMutable cf, Show cf, 
         ArithUpDn.RoundedRealInPlace cf) => 
        (ShowInternals (Poly cf))
    where
    type ShowInternalsIndicator (Poly cf) = 
        (Bool, ShowInternalsIndicator cf)
    defaultShowIndicator p =
        (False, defaultShowIndicator $ peekConst p)
    showInternals (showChebyshevTerms, coeffShowIndicator) p =
        runST $
            do
            pM <- unsafeMakeMutable p
            (Var maxArity) <- peekArityM pM
            pText <- showPolyWithVars pM coeffShowIndicator (varNamesArity maxArity)
            case showChebyshevTerms of
                True ->
                    do
                    tText <- showPolyChebTermsWithVars pM coeffShowIndicator (varNamesArity maxArity)
                    return $ pText ++ "[" ++ tText ++ "]"
                False ->
                    return pText
        where
        varNamesArity maxArity = varNames (fromIntegral maxArity)
        varNames arity = 
            take arity $ 
                ["x", "y", "z"] 
                ++ (map (\n -> "v" ++ show n ) [3..(arity - 1)])
        
showPolyWithVars :: 
    (CanBeMutable cf, ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
    PolyM cf s -> 
    (ShowInternalsIndicator cf) -> 
    [HVar] -> 
    ST s String
showPolyWithVars pM@(PolyM p) coeffShowIndicator varNames =
    do
    errorM <- peekErrorM pM
    errorBound <- unsafeReadMutable errorM
    let errorText = " ± " ++ (showInternals coeffShowIndicator errorBound) 
    case errorBoundKnownToBeZero errorBound of
        True -> return polyText
        _ -> return $ polyText ++ errorText
    where
    errorBoundKnownToBeZero errorBound =
        case errorBound ==? zero of Just True -> True; _ -> False
    polyText 
        = 
        showSymbolicPoly (showInternals coeffShowIndicator) defaultShowVar pSymb
    pSymb 
        =
        evalAtPtChebBasis 
            p
            (map hpolyVar varNames) 
            hpolyOne
            (hpolyAdd (<+>))
            (hpolySubtr neg (<+>))
            (hpolyMult (<+>) (<*>))
            (\coeff -> hpolyConst $ Interval coeff coeff)
        
showPolyChebTermsWithVars :: 
    (CanBeMutable cf, ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
    PolyM cf s -> 
    (ShowInternalsIndicator cf) -> 
    [HVar] -> 
    ST s String
showPolyChebTermsWithVars pM@(PolyM p) coeffShowIndicator varNames =
    do
    errorM <- peekErrorM pM
    errorBound <- unsafeReadMutable errorM
    let errorText = " ± " ++ (showInternals coeffShowIndicator errorBound) 
    case errorBoundKnownToBeZero errorBound of
        True -> return polyText
        _ -> return $ polyText ++ errorText
    where
    errorBoundKnownToBeZero errorBound =
        case errorBound ==? zero of Just True -> True; _ -> False
    polyText 
        = 
        showSymbolicPoly (showInternals coeffShowIndicator) chebyShowVar pSymb
    pSymb 
        =
        evalAtPtPowerBasis 
            p
            (map hpolyVar varNames) 
            hpolyOne
            (hpolyAdd (<+>))
            (hpolyMult (<+>) (<*>))
            (\coeff -> hpolyConst $ Interval coeff coeff)
        
chebyShowVar var power = 
    "T_" ++ show power ++ "(" ++ var ++ ")"

