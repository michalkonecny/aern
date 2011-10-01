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
import qualified Data.IntMap as IntMap
    
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

{- TODO: 
  change to a sparse representation and introduce variable-less polynomials:
-} 

data IntPolyTerms var cf = 
        IntPolyC -- constant
            {
                intpoly_value :: cf -- no variables, only one constant term
            }
    |   IntPolyV  -- a proper polynomial
            {
                intpoly_mainvar :: var, -- name of the main variable
                intpoly_powercoeffs :: IntMap.IntMap (IntPolyTerms var cf) 
                  -- coefficients of powers of the main variable as polynomials in other variables
                  -- often converted to a descending association list to evaluate using the Horner scheme
            }

            
instance (Show var, Show cf) => (Show (IntPolyTerms var cf))
    where
    show (IntPolyC val)
        = "C{" ++ show val ++ "}"
    show (IntPolyV x polys)
        = "V{" ++ show x ++ "/" ++ show (IntMap.toAscList polys) ++ "}"
    
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
    aux [] p@(IntPolyC _) = p
    aux (cvar : rest) (IntPolyV tvar polys)
        | cvar == tvar =
            (IntPolyV tvar $ IntMap.map (aux rest) polys)
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
    pn p@(IntPolyC val) = p
    pn (IntPolyV x polys) 
        = IntPolyV x $ IntMap.filter (not . termsIsZero) $ IntMap.map pn polys

polyIsZero ::
    (ArithInOut.RoundedReal cf) => 
    IntPoly var cf -> Bool
polyIsZero (IntPoly _ terms)
    = termsIsZero terms

termsIsZero ::
    (ArithInOut.RoundedReal cf) => 
    IntPolyTerms var cf -> Bool
termsIsZero (IntPolyC val) = (val |==? zero) == Just True
termsIsZero (IntPolyV x polys) = 
    case IntMap.toAscList polys of
        [] -> True
        [(0,p)] -> termsIsZero p
        _ -> False

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
        mkConstPoly [] = IntPolyC value
        mkConstPoly (var:rest) = IntPolyV var $ IntMap.singleton 0 (mkConstPoly rest)

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
        mkProj (cvar : rest)
            | cvar == var = IntPolyV var $ IntMap.fromAscList $ [(1, constR one)]
            | otherwise = IntPolyV cvar $ IntMap.singleton 0 (mkProj rest)
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
    sp otherVars (IntPolyC value) 
        = otherVars ++ (showCoeff value)
    sp otherVars (IntPolyV var polys)
        = intercalate " + " $ map showTerm $ IntMap.toAscList $ polys
        where
        showTerm (n,p) = sp (otherVars ++ showVarPower n) p
        showVarPower 0 = ""
        showVarPower 1 = showVar var
        showVarPower n = showVar var ++ "^" ++ show n
    