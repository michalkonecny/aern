{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basis
    Description :  datatype of polynomials with interval coefficients  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Datatype of polynomials with interval coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
--    (
--    )
where
    
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.ShowInternals

import qualified Data.Map as Map
    
import Data.List (intercalate)
    
{--- poor man's multi-variate polynomials  ---}
data IntPoly var cf = 
        IntPolyG -- uni-variate
            {
                intpoly_cfg :: (IntPolyCfg var cf), -- configuration
                intpoly_mainvar :: var, -- name of the sole variable
                intpoly_coeffs :: [cf] -- coefficients, constant term last
            }
    |   IntPolyV  -- multi-variate poly
            {
                intpoly_cfg :: (IntPolyCfg var cf), -- configuration
                intpoly_mainvar :: var, -- name of the sole variable
                intpoly_vars :: [IntPoly var cf] -- coefficients of the main variable as polynomials in other variables
            }
            
    deriving Show
    
data IntPolyCfg var cf =
    IntPolyCfg
    {
        ipolycfg_vars :: [var], -- arity and variable order
        ipolycfg_doms :: [(cf, cf)], -- domain of each variable
        ipolycfg_sample_cf :: cf,  -- sample coefficient for type inference
        ipolycfg_maxdeg :: Int, -- maximum degree of each term
        ipolycfg_maxsize :: Int -- maximum term size
    }
    deriving Show
    
polyNormalise ::
    (ArithInOut.RoundedReal cf) => 
    IntPoly var cf -> IntPoly var cf
polyNormalise (IntPolyG cfg x coeffs) 
    = IntPolyG cfg x $ removeZeros coeffs
    where
    removeZeros [c] = [c]
    removeZeros coeffs@(c : rest)
        | isZero c = removeZeros rest
        | otherwise = coeffs
        where
        isZero c = (c |==? zero) == Just True
polyNormalise (IntPolyV cfg x polys) 
    = IntPolyV cfg x $ removeZeros $ map polyNormalise polys
    where
    removeZeros [p] = [p]
    removeZeros polys@(p : rest)
        | isZeroPoly p = removeZeros rest
        | otherwise = polys

isZeroPoly ::
    (ArithInOut.RoundedReal cf) => 
    IntPoly var cf -> Bool
isZeroPoly (IntPolyG cfg x [c]) = (c |==? zero) == Just True
isZeroPoly (IntPolyV cfg x [p]) = isZeroPoly p
isZeroPoly _ = False

instance (Ord var, ArithInOut.RoundedReal cf) => (HasDomainBox (IntPoly var cf))
    where
    type (Domain (IntPoly var cf)) = cf
    type (Var (IntPoly var cf)) = var
    type (VarBox (IntPoly var cf)) = Map.Map var
    getSampleDomValue (IntPolyG cfg _ _) = ipolycfg_sample_cf cfg
    getSampleDomValue (IntPolyV cfg _ _) = ipolycfg_sample_cf cfg
--    getNVariables _ = error "operation getNVariables not implemented for IntPoly"
    getDomainBox (IntPolyG cfg _ _) = Map.fromList $ zip vars doms
        where
        vars = ipolycfg_vars cfg
        doms = ipolycfg_doms cfg
    defaultDomSplit _ (domL, domR) =
        ((domL, domM), (domM, domR))
        where
        domM = domL <+> domR

instance (HasSizeLimits (IntPoly var cf)) 
    where
    type (SizeLimits (IntPoly var cf)) = IntPolyCfg var cf
    defaultSizes = getSizeLimits 
    getSizeLimits (IntPolyG cfg _ _) = cfg
    getSizeLimits (IntPolyV cfg _ _) = cfg
    changeSizeLimits _ =
        error $ "changeSizeLimits not implemented for IntPoly"

instance 
    (Ord var, ArithInOut.RoundedReal cf) => 
    (HasConstFns (IntPoly var cf))
    where
    newConstFn cfg _ value = mkConstPoly $ ipolycfg_vars cfg
        where
        mkConstPoly [var] = IntPolyG cfg var [value]
        mkConstPoly (var:rest) = IntPolyV cfg var [mkConstPoly rest]

instance 
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf) => 
    (HasProjections (IntPoly var cf))
    where
    newProjection cfg dombox var =
        mkProj vars
        where
        vars = ipolycfg_vars cfg
        mkProj [] = 
            error $ 
                "IntPoly: newProjection: variable " ++ show var 
                ++ " not among specified variables " ++ show vars
        mkProj [cvar] | cvar == var = IntPolyG cfg var [one, zero]
        mkProj (cvar : rest)
            | cvar == var = IntPolyV cfg var [constR one, constR zero]
            | otherwise = IntPolyV cfg var [mkProj rest]
            where
            constR = newConstFn cfgR dombox
            cfgR = cfg { ipolycfg_vars = rest }
-- examples from SpringMassV.hs:
--        y0    = V "u" [V "y0" [G "y0Der" [one], G "y0Der" [zero]]]
--        y0Der = V "u" [V "y0" [G "y0Der" [one,zero]]]
instance
    (Show var, ShowInternals cf) =>
    (ShowInternals (IntPoly var cf))
    where
    type ShowInternalsIndicator (IntPoly var cf) 
        = ShowInternalsIndicator cf
    defaultShowIndicator (IntPolyG cfg var _) 
        = defaultShowIndicator $ ipolycfg_sample_cf cfg
    defaultShowIndicator (IntPolyV cfg var _) 
        = defaultShowIndicator $ ipolycfg_sample_cf cfg
    showInternals cfIndicator
        = showPoly (show) (showInternals cfIndicator)
    
showPoly ::
    (var -> String) -> 
    (cf -> String) -> 
    (IntPoly var cf -> String)
showPoly showVar showCoeff poly =
    sp "" poly
    where
    sp otherVars (IntPolyG _ var coeffs) 
        = intercalate " + " $ zipWith showTerm coeffs [degree,degree-1..0]
        where
        degree = length coeffs - 1
        showTerm coeff 0 
            = 
            case otherVars of
                "" -> showCoeff coeff
                _ -> showCoeff coeff ++ "*" ++ otherVars
        showTerm coeff n = showCoeff coeff ++ "*" ++ otherVars ++ (showVarPower n)
        showVarPower 0 = ""
        showVarPower 1 = showVar var
        showVarPower n = showVar var ++ "^" ++ show n
    sp otherVars (IntPolyV _ var polys)
        = intercalate " + " $ zipWith showTerm polys [degree,degree-1..0]
        where
        degree = length polys - 1
        showTerm p n = sp (otherVars ++ showVarPower n) p
        showVarPower 0 = ""
        showVarPower 1 = showVar var
        showVarPower n = showVar var ++ "^" ++ show n
    