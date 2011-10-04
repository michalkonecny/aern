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
    
import Data.List (intercalate, sortBy)
    
{--- multi-variate polynomials, variable-asymmetric representation  ---}
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
        IntPolyC -- constant
            {
                intpoly_value :: cf -- no variables, only one constant term
            }
    |   IntPolyV  -- a proper polynomial
            {
                intpoly_mainVar :: var, -- name of the main variable
                intpoly_pwrCoeffs :: IntPolyPowers var cf 
                  -- coefficients of powers of the main variable as polynomials in other variables
                  -- often converted to a descending association list to evaluate using the Horner scheme
            }

type IntPolyPowers var cf = IntMap.IntMap (IntPolyTerms var cf) 

powersMapCoeffs :: (cf -> cf) -> IntPolyPowers var cf -> IntPolyPowers var cf
powersMapCoeffs f pwrCoeffs = IntMap.map (termsMapCoeffs f) pwrCoeffs

termsMapCoeffs :: (cf -> cf) -> IntPolyTerms var cf -> IntPolyTerms var cf
termsMapCoeffs f (IntPolyC val) = IntPolyC $ f val
termsMapCoeffs f (IntPolyV var polys) = IntPolyV var $ powersMapCoeffs f polys


polySwapFirstTwoVars :: IntPoly var cf -> IntPoly var cf
polySwapFirstTwoVars (IntPoly cfg terms) =
    IntPoly cfgSwapped $ swappedTerms
    where
    swappedTerms = termsSwapFirstTwoVars vars terms
    vars = ipolycfg_vars cfg
    doms = ipolycfg_doms cfg
    cfgSwapped =
        cfg { ipolycfg_vars = swapFirstTwo vars, ipolycfg_doms = swapFirstTwo doms }
    swapFirstTwo :: [a] -> [a]
    swapFirstTwo (e1 : e2 : rest) = e2 : e1 : rest

termsSwapFirstTwoVars :: [var] -> IntPolyTerms var cf -> IntPolyTerms var cf
termsSwapFirstTwoVars (var1 : var2 : _) (IntPolyV _ polys) = 
    IntPolyV var2 $ newPolys
    where
    newPolys =
        IntMap.fromAscList $
            map mkTerms $ groupByFst sortedSwappedPowerAssocs
        where
        mkTerms (n1, assocs) = (n1, IntPolyV var1 $ IntMap.fromAscList assocs) 
    sortedSwappedPowerAssocs = sortBy compareFst swappedPowerAssocs
    swappedPowerAssocs = [(n2, (n1, coeff)) | (n1, list1) <- powerAssocs, (n2, coeff) <- list1]
    powerAssocs = 
        map (\(n1, IntPolyV _ polys2) -> (n1, IntMap.toAscList polys2)) $ 
            IntMap.toAscList polys
    compareFst (a,_) (b,_) = compare a b

groupByFst :: (Eq a) => [(a,b)] -> [(a,[b])]
groupByFst [] = []
groupByFst ((key, val) : assocs) = aux key [val] assocs
    where
    aux prevKey prevVals [] = [(prevKey, reverse prevVals)]
    aux prevKey prevVals ((key, val) : rest)
        | key == prevKey = aux prevKey (val : prevVals) rest
        | otherwise = (prevKey, reverse prevVals) : (aux key [val] rest)

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

{-- Internal checks and normalisation --}

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

{-- Order-related ops --}

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

-- TODO add instances for order-related type classes

joinTerms (IntPolyC c1) (IntPolyC c2) = IntPolyC $ c1 </\> c2
joinTerms (IntPolyV var terms1) (IntPolyV _ terms2) =
    IntPolyV var $ IntMap.union commonCoeffs allCoeffsWithZero
    where
    commonCoeffs =
        IntMap.intersectionWith joinTerms terms1 terms2
    allCoeffsWithZero = 
        powersMapCoeffs (</\> zero) $ IntMap.union terms1 terms2
        

{-- Basic function-approximation specific ops --}

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
    newConstFn cfg _ value = IntPoly cfg $ mkConstTerms value $ ipolycfg_vars cfg

mkConstTerms value vars = aux vars
    where
    aux [] = IntPolyC value
    aux (var:rest) = IntPolyV var $ IntMap.singleton 0 (aux rest)

instance 
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf) => 
    (HasProjections (IntPoly var cf))
    where
    newProjection cfg dombox var =
        IntPoly cfg $ mkProjTerms var vars
        where
        vars = ipolycfg_vars cfg
        
mkProjTerms var vars = aux vars
    where
    aux [] = 
        error $ 
            "IntPoly: newProjection: variable " ++ show var 
            ++ " not among specified variables " ++ show vars
    aux (cvar : rest)
        | cvar == var = IntPolyV var $ IntMap.fromAscList $ [(1, mkConstTerms one rest)]
        | otherwise = IntPolyV cvar $ IntMap.singleton 0 (aux rest)
            
            
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
    sp vars (IntPolyC value) 
        = (showCoeff value) ++ vars
    sp otherVars (IntPolyV var polys)
        = intercalate " + " $ map showTerm $ reverse $ IntMap.toAscList $ polys
        where
        showTerm (n,p) = sp (otherVars ++ showVarPower n) p
        showVarPower 0 = ""
        showVarPower 1 = showVar var
        showVarPower n = showVar var ++ "^" ++ show n
    
