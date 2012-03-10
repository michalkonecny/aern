{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.IntPoly
    Description :  datatype of polynomials and related structure functions  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Datatype of polynomials and related structure functions.
-}

module Numeric.AERN.Poly.IntPoly.IntPoly
--    (
--    )
where
    
import Numeric.AERN.Poly.IntPoly.Config
    
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures (HasImprecision(..))

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort ((|==?))

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Consistency

import qualified Data.IntMap as IntMap

import Data.List (intercalate)

    
{-| 
    Multi-variate polynomials using a representation that
    depends on a specific ordering of variables.  The first variable
    is called the main variable. The polynomial is represented
    as a uni-variate polynomial in the main variable whose
    coefficients are polynomials in the remaining variables.
-}
data IntPoly var cf =
    IntPoly
        {
            intpoly_cfg :: IntPolyCfg var cf, 
            intpoly_terms :: IntPolyTerms var cf
        }

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

type IntPolyPowers var cf = 
    IntMap.IntMap (IntPolyTerms var cf) 


{-- formatting --}

instance 
    (Show var, Show cf, ArithInOut.RoundedReal cf) => 
    Show (IntPoly var cf)
    where
    show p@(IntPoly cfg terms)
        = "IntPoly{" ++ showPoly show show p ++ "; " ++ show cfg ++ "; " ++ show terms ++ "}" 

instance 
    (Show var, Show cf) 
    => 
    (Show (IntPolyTerms var cf))
    where
    show = showTerms show
    
showTerms ::
    Show var
    =>
    (cf -> String) -> 
    IntPolyTerms var cf -> String
showTerms showCoeff (IntPolyC val)
    = "C{" ++ showCoeff val ++ "}"
showTerms showCoeff (IntPolyV x powers)
    = "V{" ++ show x ++ "/" ++ showPowers ++ "}"
    where
    showPowers =
        intercalate ", " $ map showPower $ IntMap.toAscList powers
    showPower (n, terms) =
        "^" ++ show n ++ "->" ++ showTerms showCoeff terms 
    
instance
    (Show var, Show cf, ShowInternals cf, 
     HasZero cf, RefOrd.PartialComparison cf) 
    =>
    (ShowInternals (IntPoly var cf))
    where
    type ShowInternalsIndicator (IntPoly var cf) 
        = (ShowInternalsIndicator cf, Bool)
    defaultShowIndicator (IntPoly cfg _) 
        = (defaultShowIndicator $ ipolycfg_sample_cf cfg, False)
    showInternals (cfIndicator, shouldShowTerms) p@(IntPoly _cfg terms)
        | shouldShowTerms =
            showPoly show showCf p ++ "[" ++ showTerms showCf terms ++ "]"
        | otherwise = 
            showPoly show showCf p
        where
        showCf = showInternals cfIndicator
    
showPoly ::
    (HasZero cf, RefOrd.PartialComparison cf)
    =>
    (var -> String) -> 
    (cf -> String) -> 
    (IntPoly var cf -> String)
showPoly showVar showCoeff (IntPoly cfg terms) =
    sp "" domsLE terms
    where
    domsLE = ipolycfg_domsLE cfg
    sp vars _ (IntPolyC value) 
        = (showCoeff value) ++ vars
    sp otherVars (domLE : restDomsLE) (IntPolyV var powers)
        = intercalate " + " $ map showTerm $ reverse $ IntMap.toAscList $ powers
        where
        showTerm (n,p) = sp (otherVars ++ showVarPower n) restDomsLE p
        showVarPower 0 = ""
        showVarPower 1 = showVarShift
        showVarPower n = showVarShift ++ "^" ++ show n
        showVarShift =
            case domLE |==? zero domLE of
                Just True -> showVar var
                _ -> "(" ++ showVar var ++ ")"
    sp _ _ _ =
        error $ "aern-poly: internal error in IntPoly.showPoly" 
    
{-- simple spine-crawling operations --}

termsCollectCoeffsWith :: 
    ([Int] -> cf -> a) {-^ a function for creating result values from coefficients and their term powers -} -> 
    IntPolyTerms var cf -> 
    [a]
termsCollectCoeffsWith fn terms = aux [] terms
    where
    aux prevDegrees (IntPolyC cf) = [fn (reverse prevDegrees) cf] 
    aux prevDegrees (IntPolyV _var powers) = 
        IntMap.fold (++) [] $ IntMap.mapWithKey applyAux powers
        where
        applyAux degree = aux (degree : prevDegrees)

--countTermsCoeffsSatisfying cond (IntPolyC cf) 
--    | cond cf = 1
--    | otherwise = 0
--countTermsCoeffsSatisfying cond (IntPolyV x polys) = 
--    IntMap.fold (+) 0 $ IntMap.map (countTermsCoeffsSatisfying cond) polys

powersMapCoeffs :: (cf -> cf) -> IntPolyPowers var cf -> IntPolyPowers var cf
powersMapCoeffs f pwrCoeffs = IntMap.map (termsMapCoeffs f) pwrCoeffs

termsMapCoeffs :: (cf -> cf) -> IntPolyTerms var cf -> IntPolyTerms var cf
termsMapCoeffs f (IntPolyC val) = IntPolyC $ f val
termsMapCoeffs f (IntPolyV var polys) = IntPolyV var $ powersMapCoeffs f polys

polyMapCoeffs :: (cf -> cf) -> IntPoly var cf -> IntPoly var cf
polyMapCoeffs f (IntPoly cfg terms) = IntPoly cfg $ termsMapCoeffs f terms

termsMapCoeffsWithDegrees ::
    ([Int] -> cf1 -> cf2) {-^ mapping function whose first argument is the list of degrees for each variable in _reverse_ order -} ->
    (IntPolyTerms var cf1) ->
    (IntPolyTerms var cf2)
termsMapCoeffsWithDegrees fn terms = aux [] terms    
    where
    aux prevDegrees (IntPolyC cf) = IntPolyC $ fn prevDegrees cf
    aux prevDegrees (IntPolyV x polys) =
        IntPolyV x $ IntMap.mapWithKey applyAux polys
        where
        applyAux degree = aux (degree : prevDegrees)
        


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
    aux _ = 
        error $ "aern-intpoly internal error: Poly: termsJoinWith used with illegal values"

polyGetEndpointsOutEff ::
    RefOrd.IntervalLike cf 
    =>
    RefOrd.GetEndpointsEffortIndicator cf -> 
    IntPoly var cf -> 
    (IntPoly var cf, IntPoly var cf)
polyGetEndpointsOutEff effGetE p =
    polySplitWith (RefOrd.getEndpointsOutEff effGetE) p

{-- Internal checks and normalisation --}
polyNormalise ::
    (ArithInOut.RoundedReal cf)
    => 
    IntPoly var cf -> IntPoly var cf
polyNormalise (IntPoly cfg poly)
    = IntPoly cfg (termsNormalise poly) 

termsNormalise ::
    (ArithInOut.RoundedReal cf) 
    =>
    IntPolyTerms var cf -> IntPolyTerms var cf
termsNormalise poly =
    pn poly
    where    
    pn p@(IntPolyC _val) = p
    pn (IntPolyV x polys)
        = IntPolyV x $ IntMap.filterWithKey nonZeroOrConst $ IntMap.map pn polys
        where
        nonZeroOrConst degree subTerms =
            degree == 0 || (not $ termsIsZero subTerms)  

instance
    (HasLegalValues cf, Show cf, 
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     Show var, Ord var) 
    =>
    HasLegalValues (IntPoly var cf)
    where
    maybeGetProblem (IntPoly cfg terms) = 
        maybeGetProblemForTerms cfg terms
    
maybeGetProblemForTerms :: 
    (HasLegalValues cf, Show cf, 
     ArithInOut.RoundedReal cf,
     HasConsistency cf,
     Show var, Ord var) 
    =>
    IntPolyCfg var cf -> 
    IntPolyTerms var cf -> 
    Maybe [Char]
maybeGetProblemForTerms cfg terms
    =
    aux vars terms
    where
    vars = ipolycfg_vars cfg
    intro = "cfg = " ++ show cfg ++ "; terms = " ++ show terms ++ ": "
    aux [] _p@(IntPolyC value) =
        case fmap ((intro ++ "problem with coefficient: ") ++) $ maybeGetProblem value of
            Nothing -> 
                case isConsistentEff (consistencyDefaultEffort value) value of
                    Just False -> Just $ intro ++ "inconsistent coefficient: " ++ show value
                    _ -> Nothing 
            problem -> problem
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
    findFirstJust (j@(Just _) : _rest) = j
    findFirstJust (Nothing:rest) = findFirstJust rest    
    findFirstJust [] = Nothing

{-- Order-related ops --}

{-| 
    Swaps the consistency of all coefficients.
    BEWARE: This function typically invalidates the assumption that all coefficients are consistent.
    
    This operations is currently used only to define inner-rounded evaluation
    of an IntPoly over a point or interval. 
-}
flipConsistencyPoly :: 
    HasAntiConsistency cf 
    =>
    IntPoly var cf -> IntPoly var cf
flipConsistencyPoly (IntPoly cfg terms) =
    IntPoly cfg $ termsMapCoeffs flipConsistency terms 
    
polyIsExactEff ::
    (HasImprecision cf)
    =>
    (ImprecisionEffortIndicator cf) ->
    (IntPoly var cf) ->
    Maybe Bool
polyIsExactEff effImpr _p@(IntPoly _ terms) = termsAreExactEff effImpr terms 

termsAreExactEff ::
    (HasImprecision cf)
    =>
    (ImprecisionEffortIndicator cf) ->
    (IntPolyTerms var cf) ->
    Maybe Bool
termsAreExactEff effImpr (IntPolyC val) = isExactEff effImpr val 
termsAreExactEff effImpr (IntPolyV _var polys) =
    do -- the Maybe monad, ie if any coefficient returns Nothing, so does this function 
    results <- mapM (termsAreExactEff effImpr) $ IntMap.elems polys
    return $ and results

polyIsZero ::
    (ArithInOut.RoundedReal cf) 
    => 
    IntPoly var cf -> Bool
polyIsZero (IntPoly _ terms)
    = termsIsZero terms

termsIsZero ::
    (ArithInOut.RoundedReal cf) 
    => 
    IntPolyTerms var cf -> Bool
termsIsZero (IntPolyC val) = (val |==? (zero val)) == Just True
termsIsZero (IntPolyV _var polys) = 
    case IntMap.toAscList polys of
        [] -> True
        [(0,p)] -> termsIsZero p
        _ -> False
