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
    IntPoly
        {
            intpoly_cfg :: IntPolyCfg var cf, 
            intpoly_terms :: IntPolyTerms var cf
        }

instance (Show var, Show cf) => Show (IntPoly var cf)
    where
    show (IntPoly cfg terms)
        = "IntPoly{" ++ show cfg ++ "; " ++ show terms ++ "}" 

data IntPolyTerms var cf = 
        IntPolyG -- uni-variate
            {
                intpoly_mainvar :: var, -- name of the sole variable
                intpoly_coeffs :: [cf] -- coefficients, constant term last
            }
    |   IntPolyV  -- multi-variate poly
            {
                intpoly_mainvar :: var, -- name of the sole variable
                intpoly_vars :: [IntPolyTerms var cf] -- coefficients of the main variable as polynomials in other variables
            }
            
instance (Show var, Show cf) => (Show (IntPolyTerms var cf))
    where
    show (IntPolyG x coeffs)
        = "G{" ++ show x ++ "/" ++ show coeffs ++ "}"
    show (IntPolyV x polys)
        = "V{" ++ show x ++ "/" ++ show polys ++ "}"
    
data IntPolyCfg var cf =
    IntPolyCfg
    {
        ipolycfg_vars :: [var], -- arity and variable order
        ipolycfg_doms :: [(cf, cf)], -- domain of each variable
        ipolycfg_sample_cf :: cf,  -- sample coefficient for type inference
        ipolycfg_maxdeg :: Int, -- maximum degree of each term
        ipolycfg_maxsize :: Int -- maximum term size
    }

cfgRemVar cfg = cfg
        { 
            ipolycfg_vars = tail $ ipolycfg_vars cfg, 
            ipolycfg_doms = tail $ ipolycfg_doms cfg 
        }

instance (Show var, Show cf) => Show (IntPolyCfg var cf)
    where
    show (IntPolyCfg vars doms _ maxdeg maxsize) 
        = "cfg{" ++ (show $ zip vars doms) ++ ";" ++ show maxdeg ++ "/" ++ show maxsize ++ "}"

checkPoly (IntPoly cfg terms)
    =
    IntPoly cfg $ checkTerms cfg terms
    
checkTerms cfg terms
    =
    aux vars terms
    where
    vars = ipolycfg_vars cfg
    aux [cvar] p@(IntPolyG tvar coeffs) | cvar == tvar = p
    aux (cvar : rest) (IntPolyV tvar polys)
        | cvar == tvar =
            (IntPolyV tvar $ map (aux rest) polys)
    aux _ _ = 
        error $ "checkTerms failed for cfg = " ++ show cfg ++ " and term = " ++ show terms 
    
polyNormalise ::
    (ArithInOut.RoundedReal cf) => 
    IntPoly var cf -> IntPoly var cf
polyNormalise (IntPoly cfg poly)
    = IntPoly cfg (termsNormalise cfg poly) 

termsNormalise cfg poly =
    pn poly
    where    
    pn (IntPolyG x coeffs)
        = IntPolyG x $ removeZeros coeffs
        where
        removeZeros [c] = [c]
        removeZeros coeffs@(c : rest)
            | isZero c = removeZeros rest
            | otherwise = coeffs
            where
            isZero c = (c |==? zero) == Just True
    pn (IntPolyV x polys) 
        = IntPolyV x $ removeZeros $ map pn polys
        where
        removeZeros [p] = [p]
        removeZeros polys@(p : rest)
            | termsIsZero p = removeZeros rest
            | otherwise = polys

polyIsZero ::
    (ArithInOut.RoundedReal cf) => 
    IntPoly var cf -> Bool
polyIsZero (IntPoly _ poly)
    = termsIsZero poly

termsIsZero ::
    (ArithInOut.RoundedReal cf) => 
    IntPolyTerms var cf -> Bool
termsIsZero (IntPolyG x [c]) = (c |==? zero) == Just True
termsIsZero (IntPolyV x [p]) = termsIsZero p
termsIsZero _ = False

instance (Ord var, ArithInOut.RoundedReal cf) => (HasDomainBox (IntPoly var cf))
    where
    type (Domain (IntPoly var cf)) = cf
    type (Var (IntPoly var cf)) = var
    type (VarBox (IntPoly var cf)) = Map.Map var
    getSampleDomValue (IntPoly cfg _) = ipolycfg_sample_cf cfg
--    getNVariables _ = error "operation getNVariables not implemented for IntPoly"
    getDomainBox (IntPoly cfg _) = Map.fromList $ zip vars doms
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
    getSizeLimits (IntPoly cfg _) = cfg
    changeSizeLimits _ =
        error $ "changeSizeLimits not implemented for IntPoly"

instance 
    (Ord var, ArithInOut.RoundedReal cf) => 
    (HasConstFns (IntPoly var cf))
    where
    newConstFn cfg _ value = IntPoly cfg $ mkConstPoly $ ipolycfg_vars cfg
        where
        mkConstPoly [var] = IntPolyG var [value]
        mkConstPoly (var:rest) = IntPolyV var [mkConstPoly rest]

instance 
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf) => 
    (HasProjections (IntPoly var cf))
    where
    newProjection cfg dombox var =
        IntPoly cfg $ mkProj vars
        where
        vars = ipolycfg_vars cfg
        mkProj [] = 
            error $ 
                "IntPoly: newProjection: variable " ++ show var 
                ++ " not among specified variables " ++ show vars
        mkProj [cvar] | cvar == var = IntPolyG var [one, zero]
        mkProj (cvar : rest)
            | cvar == var = IntPolyV var [constR one, constR zero]
            | otherwise = IntPolyV cvar [mkProj rest]
            where
            constR c = intpoly_terms $ newConstFn cfgR dombox c
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
    defaultShowIndicator (IntPoly cfg _) 
        = defaultShowIndicator $ ipolycfg_sample_cf cfg
    showInternals cfIndicator
        = showPoly (show) (showInternals cfIndicator)
    
showPoly ::
    (var -> String) -> 
    (cf -> String) -> 
    (IntPoly var cf -> String)
showPoly showVar showCoeff (IntPoly cfg poly) =
    sp "" poly
    where
    sp otherVars (IntPolyG var coeffs) 
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
    sp otherVars (IntPolyV var polys)
        = intercalate " + " $ zipWith showTerm polys [degree,degree-1..0]
        where
        degree = length polys - 1
        showTerm p n = sp (otherVars ++ showVarPower n) p
        showVarPower 0 = ""
        showVarPower 1 = showVar var
        showVarPower n = showVar var ++ "^" ++ show n
    