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

import Numeric.AERN.Basics.ShowInternals

import qualified Data.Map as Map
    
import Data.List (intercalate)
    
{--- poor man's multi-variate polynomials  ---}
data IntPoly var cf = 
        IntPolyG -- uni-variate 
            (IntPolyCfg var cf) -- configuration
            var -- name of the sole variable
            [cf] -- coefficients, constant term last
    |   IntPolyV  -- multi-variate poly
            (IntPolyCfg var cf) -- configuration
            var -- the main variable
            [IntPoly var cf] -- coefficients of the main variable as polynomials in other variables
    deriving Show
    
data IntPolyCfg var cf =
    IntPolyCfg
    {
        ipolycfg_vars :: [var], -- arity and variable order
        ipolycfg_doms :: [Interval cf], -- domain of each variable
        ipolycfg_sample_cf :: cf,  -- sample coefficient for type inference
        ipolycfg_maxdeg :: Int, -- maximum degree of each term
        ipolycfg_maxsize :: Int -- maximum term size
    }
    deriving Show
    

instance (HasDomainBox (IntPoly var cf))
    where
    type (Domain (IntPoly var cf)) = cf
    type (Var (IntPoly var cf)) = var
    type (VarBox (IntPoly var cf)) = Map.Map var
    getSampleDomValue (IntPolyG cfg _ _) = ipolycfg_sample_cf cfg
    getSampleDomValue (IntPolyV cfg _ _) = ipolycfg_sample_cf cfg
    getNVariables _ = error "operation getNVariables not implemented for IntPoly"
    getDomainBox (IntPolyG cfg _ _) =

instance (HasSizeLimits (IntPoly var cf)) 
    where
    type (SizeLimits (IntPoly var cf)) = IntPolyCfg var cf
    defaultSizes = getSizeLimits 
    getSizeLimits (IntPolyG cfg _ _) = cfg
    getSizeLimits (IntPolyV cfg _ _) = cfg
    changeSizeLimits _ =
        error $ "changeSizeLimits not implemented for IntPoly"

instance (HasConstFns (IntPoly var cf))
    where
    

instance
    (ShowInternals var, ShowInternals cf) =>
    (ShowInternals (IntPoly var cf))
    where
    type ShowInternalsIndicator (IntPoly var cf) 
        = (ShowInternalsIndicator var, ShowInternalsIndicator cf)
    defaultShowIndicator (IntPolyG cfg var _) 
        = (defaultShowIndicator var, defaultShowIndicator $ ipolycfg_sample_cf cfg)
    defaultShowIndicator (IntPolyV cfg var _) 
        = (defaultShowIndicator var, defaultShowIndicator $ ipolycfg_sample_cf cfg)
    showInternals (varIndicator, cfIndicator)
        = showPoly (showInternals varIndicator) (showInternals cfIndicator)
    
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
    