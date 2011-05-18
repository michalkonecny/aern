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

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly
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
        (Show (PolyPure cf)) 
    where
    show = showUsingShowInternals

instance
        (ShowInternals cf, Storable cf,
         CanBeMutable cf, Show cf, 
         ArithUpDn.RoundedRealInPlace cf) => 
        (ShowInternals (PolyPure cf))
    where
    type ShowInternalsIndicator (PolyPure cf) = 
        (Bool, ShowInternalsIndicator cf)
    defaultShowIndicator p =
        runST $
            do
            pM <- unsafeMakeMutable p
            constM <- peekConst pM
            const <- unsafeReadMutable constM
            return $ (False, defaultShowIndicator const)
    showInternals (showChebyshevTerms, coeffShowIndicator) p =
        runST $
            do
            pM <- unsafeMakeMutable p
            (Var maxArity) <- peekArity pM
            pText <- showPolyFPWithVars pM coeffShowIndicator (varNamesArity maxArity)
            case showChebyshevTerms of
                True ->
                    do
                    tText <- showPolyFPChebTermsWithVars pM coeffShowIndicator (varNamesArity maxArity)
                    return $ pText ++ "[" ++ tText ++ "]"
                False ->
                    return pText
        where
        varNamesArity maxArity = varNames (fromIntegral maxArity)
        varNames arity = 
            take arity $ 
                ["x", "y", "z"] 
                ++ (map (\n -> "v" ++ show n ) [3..(arity - 1)])
        
showPolyFPWithVars :: 
    (CanBeMutable cf, ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
    PolyFP cf s -> 
    (ShowInternalsIndicator cf) -> 
    [HVar] -> 
    ST s String
showPolyFPWithVars pM coeffShowIndicator varNames =
    do
    errorM <- peekError pM
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
            pM
            (map hpolyVar varNames) 
            hpolyOne
            (hpolyAdd (<+>))
            (hpolySubtr neg (<+>))
            (hpolyMult (<+>) (<*>))
            (\coeff -> hpolyConst $ Interval coeff coeff)
        
showPolyFPChebTermsWithVars :: 
    (CanBeMutable cf, ShowInternals cf, Storable cf, ArithUpDn.RoundedReal cf) => 
    PolyFP cf s -> 
    (ShowInternalsIndicator cf) -> 
    [HVar] -> 
    ST s String
showPolyFPChebTermsWithVars pM coeffShowIndicator varNames =
    do
    errorM <- peekError pM
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
            pM
            (map hpolyVar varNames) 
            hpolyOne
            (hpolyAdd (<+>))
            (hpolyMult (<+>) (<*>))
            (\coeff -> hpolyConst $ Interval coeff coeff)
        
chebyShowVar var power = 
    "T_" ++ show power ++ "(" ++ var ++ ")"

