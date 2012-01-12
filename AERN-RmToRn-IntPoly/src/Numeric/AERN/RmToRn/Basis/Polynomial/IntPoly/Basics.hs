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
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.QuickCheck

import Test.QuickCheck

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

import Data.List (intercalate, sortBy)

    
{-| 
    Multi-variate polynomials using variable-asymmetric representation.
      
    Note that if variables have interval-like domains, each of their domains
    must be **non-negative**.  The 'RefOrd.IntervalLike' instance
    of this type is valid only with this assumtion.
-}
data IntPoly var cf =
    IntPoly
        {
            intpoly_cfg :: IntPolyCfg var cf, 
            intpoly_terms :: IntPolyTerms var cf
        }

instance (Show var, Show cf) => Show (IntPoly var cf)
    where
    show p@(IntPoly cfg terms)
        = "IntPoly{" ++ showPoly show show p ++ "; " ++ show cfg ++ "; " ++ show terms ++ "}" 

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

polyAddMainVar :: var -> cf -> IntPoly var cf -> IntPoly var cf
polyAddMainVar var dom p@(IntPoly cfg terms) =
    IntPoly cfgNew termsNew
    where
    termsNew = IntPolyV var $ IntMap.singleton 0 terms
    cfgNew = cfg 
        { 
            ipolycfg_vars = var : ipolycfg_vars cfg, 
            ipolycfg_doms = dom : ipolycfg_doms cfg 
        }

polyRenameMainVar :: var -> IntPoly var cf -> IntPoly var cf
polyRenameMainVar newVarName p@(IntPoly cfg (IntPolyV _ coeffs)) =
    IntPoly cfgNew termsNew
    where
    termsNew = IntPolyV newVarName coeffs
    cfgNew = cfg 
        { 
            ipolycfg_vars = newVarName : (tail $ ipolycfg_vars cfg) 
        }

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
        where
        compareFst (a,_) (b,_) = compare a b
    swappedPowerAssocs = [(n2, (n1, coeff)) | (n1, list1) <- powerAssocs, (n2, coeff) <- list1]
    powerAssocs = 
        map (\(n1, IntPolyV _ polys2) -> (n1, IntMap.toAscList polys2)) $ 
            IntMap.toAscList polys

groupByFst :: (Eq a) => [(a,b)] -> [(a,[b])]
groupByFst [] = []
groupByFst ((key, val) : assocs) = aux key [val] assocs
    where
    aux prevKey prevVals [] = [(prevKey, reverse prevVals)]
    aux prevKey prevVals ((key, val) : rest)
        | key == prevKey = aux prevKey (val : prevVals) rest
        | otherwise = (prevKey, reverse prevVals) : (aux key [val] rest)

instance 
    (Show var, Show cf) 
    => 
    (Show (IntPolyTerms var cf))
    where
    show (IntPolyC val)
        = "C{" ++ show val ++ "}"
    show (IntPolyV x polys)
        = "V{" ++ show x ++ "/" ++ show (IntMap.toAscList polys) ++ "}"
    
 
data IntPolyCfg var cf =
    IntPolyCfg
    {
        ipolycfg_vars :: [var], -- arity and variable order
        ipolycfg_doms :: [cf], -- domain of each variable, MUST NOT HAVE ZERO IN THE INTERIOR!!
        ipolycfg_sample_cf :: cf,  -- sample coefficient for type inference
        ipolycfg_maxdeg :: Int, -- maximum degree of each term
        ipolycfg_maxsize :: Int -- maximum term size
    }

cfgRemVar cfg = cfg
        { 
            ipolycfg_vars = tail $ ipolycfg_vars cfg, 
            ipolycfg_doms = tail $ ipolycfg_doms cfg 
        }

instance 
    (Show var, Show cf) 
    =>
    Show (IntPolyCfg var cf)
    where
    show (IntPolyCfg vars doms _ maxdeg maxsize) 
        = "cfg{" ++ (show $ zip vars doms) ++ ";" ++ show maxdeg ++ "/" ++ show maxsize ++ "}"

instance
    (RefOrd.IntervalLike cf, 
     HasOne cf, HasZero cf, HasAntiConsistency cf, 
     Arbitrary cf, GeneratableVariables var) 
    =>
    (Arbitrary (IntPolyCfg var cf))
    where
    arbitrary =
        do
        Int1To10 arity <- arbitrary
        Int1To10 maxdeg <- arbitrary
        Int1To1000 maxsizeRaw <- arbitrary
        sampleCfs <- vectorOf (50 * arity) arbitrary -- probability that too many of these are anti-consistent is negligible
        return $ mkCfg arity maxdeg maxsizeRaw sampleCfs
        where
        mkCfg arity maxdeg maxsizeRaw sampleCfs =
            IntPolyCfg
                vars doms (head sampleCfs) maxdeg (max 2 maxsizeRaw)
            where
            vars = getNVariables arity
            doms = take arity $ filter notAntiConsistent sampleCfs
            notAntiConsistent a =
                (isAntiConsistentEff eff a) == Just False
                where
                eff = consistencyDefaultEffort a
             
instance
    (Show var, Show cf,
     RefOrd.IntervalLike cf, 
     HasOne cf, HasZero cf, HasAntiConsistency cf, 
     Arbitrary cf, GeneratableVariables var) 
    =>
    (EffortIndicator (IntPolyCfg var cf))
    where
    effortIncrementVariants (IntPolyCfg vars doms sample maxdeg maxsize) =
        map recreateCfg $ effortIncrementVariants (Int1To10 maxdeg, Int1To1000 maxsize)
        where
        recreateCfg (Int1To10 md, Int1To1000 ms) =
            IntPolyCfg vars doms sample md ms 
    effortIncrementSequence (IntPolyCfg vars doms sample maxdeg maxsize) =
        map recreateCfg $ effortIncrementSequence (Int1To10 maxdeg, Int1To1000 maxsize)
        where
        recreateCfg (Int1To10 md, Int1To1000 ms) =
            IntPolyCfg vars doms sample md ms
    effortRepeatIncrement 
            (IntPolyCfg vars doms sample maxdeg1 maxsize1, 
             IntPolyCfg _ _ _ maxdeg2 maxsize2)
        =
        IntPolyCfg vars doms sample md ms
        where
        Int1To10 md = effortRepeatIncrement (Int1To10 maxdeg1, Int1To10 maxdeg2)  
        Int1To1000 ms = effortRepeatIncrement (Int1To1000 maxsize1, Int1To1000 maxsize2)  

{-- Internal checks and normalisation --}

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
        = IntPolyV x $ IntMap.filterWithKey nonZeroOrConst $ IntMap.map pn polys
        where
        nonZeroOrConst degree subTerms =
            degree == 0 || (not $ termsIsZero subTerms)  

{-- Order-related ops --}

polyIsExactEff effImpr p@(IntPoly _ terms) = termsAreExactEff effImpr terms 

termsAreExactEff ::
    (HasImprecision cf)
    =>
    (ImprecisionEffortIndicator cf) ->
    (IntPolyTerms var cf) ->
    Maybe Bool
termsAreExactEff effImpr (IntPolyC val) = isExactEff effImpr val 
termsAreExactEff effImpr (IntPolyV var polys) =
    do -- the Maybe monad, ie if any coefficient returns Nothing, so does this function 
    results <- mapM (termsAreExactEff effImpr) $ IntMap.elems polys
    return $ and results

polyIsZero ::
    (ArithInOut.RoundedReal cf) => 
    IntPoly var cf -> Bool
polyIsZero (IntPoly _ terms)
    = termsIsZero terms

termsIsZero ::
    (ArithInOut.RoundedReal cf) => 
    IntPolyTerms var cf -> Bool
termsIsZero (IntPolyC val) = (val |==? (zero val)) == Just True
termsIsZero (IntPolyV x polys) = 
    case IntMap.toAscList polys of
        [] -> True
        [(0,p)] -> termsIsZero p
        _ -> False

{-
    The following instance is meaningful only when each
    variable's domain is non-negative. 
-}
instance
    (RefOrd.IntervalLike cf, HasZero cf)
    => 
    (RefOrd.IntervalLike (IntPoly var cf))
    where
    type RefOrd.GetEndpointsEffortIndicator (IntPoly var cf) = 
        RefOrd.GetEndpointsEffortIndicator cf
    type RefOrd.FromEndpointsEffortIndicator (IntPoly var cf) = 
        RefOrd.FromEndpointsEffortIndicator cf
    getEndpointsDefaultEffort (IntPoly cfg _) =
        RefOrd.getEndpointsDefaultEffort sampleCf
        where
        sampleCf = ipolycfg_sample_cf cfg
    fromEndpointsDefaultEffort (IntPoly cfg _) =
        RefOrd.fromEndpointsDefaultEffort sampleCf
        where
        sampleCf = ipolycfg_sample_cf cfg
    getEndpointsInEff eff = polySplitWith (RefOrd.getEndpointsInEff eff)
    getEndpointsOutEff eff = polySplitWith (RefOrd.getEndpointsOutEff eff)
    fromEndpointsInEff eff pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsInEff eff) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 
    fromEndpointsOutEff eff pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsOutEff eff) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 
    getEndpointsInWithDefaultEffort = polySplitWith (RefOrd.getEndpointsInWithDefaultEffort)
    getEndpointsOutWithDefaultEffort = polySplitWith (RefOrd.getEndpointsOutWithDefaultEffort)
    fromEndpointsInWithDefaultEffort pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsInWithDefaultEffort) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 
    fromEndpointsOutWithDefaultEffort pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsOutWithDefaultEffort) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 

polySplitWith ::
    (cf -> (cf,cf)) ->
    (IntPoly var cf) -> (IntPoly var cf, IntPoly var cf)
polySplitWith splitCf (IntPoly cfg terms) = 
    (IntPoly cfg termsL, IntPoly cfg termsR)
    where
    (termsL, termsR) = termsSplitWith splitCf terms
termsSplitWith ::
    (cf -> (cf,cf)) ->
    (IntPolyTerms var cf) -> (IntPolyTerms var cf, IntPolyTerms var cf)
termsSplitWith splitCf (IntPolyC val) = 
    (IntPolyC valL, IntPolyC valR)
    where
    (valL, valR) = splitCf val
termsSplitWith splitCf (IntPolyV var polys) = 
    (IntPolyV var polysL, IntPolyV var polysR)
    where
    polysL = IntMap.map fst polysLR
    polysR = IntMap.map snd polysLR
    polysLR = IntMap.map (termsSplitWith splitCf) polys

polyJoinWith ::
    cf {-^ zero coeff -} ->
    ((cf,cf) -> cf) ->
    (IntPoly var cf, IntPoly var cf) -> (IntPoly var cf) 
polyJoinWith z joinCf (IntPoly cfg termsL, IntPoly _ termsR) =
    (IntPoly cfg terms) 
    where
    terms = termsJoinWith z joinCf (termsL, termsR)
termsJoinWith ::
    cf {-^ zero coeff -} ->
    ((cf,cf) -> cf) ->
    (IntPolyTerms var cf, IntPolyTerms var cf) -> (IntPolyTerms var cf) 
termsJoinWith z joinCf (tL, tR) =
    aux (Just tL, Just tR)
    where
    aux (Just (IntPolyC valL), Just (IntPolyC valR)) = 
        IntPolyC val
        where
        val = joinCf (valL, valR)
    aux (Nothing, Just (IntPolyC valR)) = 
        IntPolyC val
        where
        val = joinCf (z, valR)
    aux (Just (IntPolyC valL), Nothing) = 
        IntPolyC val
        where
        val = joinCf (valL, z)
    aux (Just (IntPolyV var polysL), Just (IntPolyV _ polysR)) = 
        IntPolyV var $ IntMap.map aux polys 
        where
        polys = polysLR `IntMap.union` polysLonly `IntMap.union` polysRonly
        polysLonly = IntMap.map (\l -> (Just l, Nothing)) $ polysL `IntMap.difference` polysR 
        polysRonly = IntMap.map (\r -> (Nothing, Just r)) $ polysR `IntMap.difference` polysL 
        polysLR = IntMap.intersectionWith (\l -> \r -> (Just l, Just r)) polysL polysR  
    aux (Nothing, Just (IntPolyV var polysR)) = 
        IntPolyV var $ IntMap.map (aux . addNothing) polysR 
        where
        addNothing t = (Nothing, Just t)
    aux (Just (IntPolyV var polysL), Nothing) = 
        IntPolyV var $ IntMap.map (aux . addNothing) polysL 
        where
        addNothing t = (Just t, Nothing)

--joinTerms cfg t1 t2 = termsJoinWith z (uncurry (</\>)) (t1,t2)
--    where
--    z = zero $ ipolycfg_sample_cf cfg
    

{-- Basic function-approximation specific ops --}

instance 
    (Ord var, ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    (HasDomainBox (IntPoly var cf))
    where
    type (Domain (IntPoly var cf)) = cf
    type (Var (IntPoly var cf)) = var
    type (VarBox (IntPoly var cf)) = Map.Map var
    getSampleDomValue (IntPoly cfg _) = ipolycfg_sample_cf cfg
    defaultDomSplit _ =
        defaultDomSplitUsingEndpointsDefaultEffort
    getDomainBox (IntPoly cfg _) = Map.fromList $ zip vars doms
        where
        vars = ipolycfg_vars cfg
        doms = ipolycfg_doms cfg
    getNSamplesFromDomainBox sampleP@(IntPoly cfg _) dombox n =
        getNSamplesFromDomainBoxUsingEndpointsDefaultEffort sampleDom sampleP dombox n
        where
        sampleDom = ipolycfg_sample_cf cfg
    getSampleFromInsideDomainBox sampleP@(IntPoly cfg _) dombox =
        getSampleFromInsideDomainBoxUsingEndpointsDefaultEffort sampleDom sampleP dombox
        where
        sampleDom = ipolycfg_sample_cf cfg

instance (HasSizeLimits (IntPoly var cf)) 
    where
    type (SizeLimits (IntPoly var cf)) = IntPolyCfg var cf
    defaultSizeLimits = getSizeLimits 
    getSizeLimits (IntPoly cfg _) = cfg
    changeSizeLimits cfg (IntPoly _ terms) = IntPoly cfg terms
--        error $ "changeSizeLimits not implemented for IntPoly"

instance 
    (Ord var, ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) => 
    (HasConstFns (IntPoly var cf))
    where
    newConstFn cfg _ value = IntPoly cfg $ mkConstTerms value $ ipolycfg_vars cfg

mkConstTerms value vars = aux vars
    where
    aux [] = IntPolyC value
    aux (var:rest) = IntPolyV var $ IntMap.singleton 0 (aux rest)

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    HasZero (IntPoly var cf)
    where
    zero sampleP = newConstFnFromSample sampleP $ zero sampleCf
        where
        sampleCf = getSampleDomValue sampleP
        
instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    HasOne (IntPoly var cf)
    where
    one sampleP = newConstFnFromSample sampleP $ one sampleCf
        where
        sampleCf = getSampleDomValue sampleP
        
instance 
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) => 
    (HasProjections (IntPoly var cf))
    where
    newProjection cfg dombox var =
        IntPoly cfg $ mkProjTerms cfg var vars
        where
        vars = ipolycfg_vars cfg
        
mkProjTerms cfg var vars = aux vars
    where
    aux [] = 
        error $ 
            "IntPoly: newProjection: variable " ++ show var 
            ++ " not among specified variables " ++ show vars
    aux (cvar : rest)
        | cvar == var = 
            IntPolyV var $ 
                IntMap.fromAscList $ 
                    [(1, mkConstTerms o rest),
                     (0, mkConstTerms z rest)]
        | otherwise = 
            IntPolyV cvar $ IntMap.singleton 0 (aux rest)
        where
        o = one sampleCf
        z = zero sampleCf
        sampleCf = ipolycfg_sample_cf cfg 
            
            
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
    
instance (HasLegalValues cf, Show cf, Show var, Ord var) => HasLegalValues (IntPoly var cf)
    where
    maybeGetProblem (IntPoly cfg terms) = 
        maybeGetProblemForTerms cfg terms
    
maybeGetProblemForTerms cfg terms
    =
    aux vars terms
    where
    vars = ipolycfg_vars cfg
    intro = "cfg = " ++ show cfg ++ "; terms = " ++ show terms ++ ": "
    aux [] p@(IntPolyC value) = 
        fmap ((intro ++ "problem with coefficient: ") ++) $ maybeGetProblem value
    aux (cvar : rest) (IntPolyV tvar polys)
        | cvar == tvar =
            findFirstJust $ map (aux rest) $ IntMap.elems polys
        | otherwise = 
            Just $
                intro ++ 
                "variable name mismatch: declared = " ++ show cvar ++ " actual = " ++ show tvar  
    aux [] _ =
        Just $ intro ++ "more variables than declared"  
    aux _ _ =
        Just $ intro ++ "less variables than declared"  
    findFirstJust (j@(Just _) : rest) = j
    findFirstJust (Nothing:rest) = findFirstJust rest    
    findFirstJust [] = Nothing
            