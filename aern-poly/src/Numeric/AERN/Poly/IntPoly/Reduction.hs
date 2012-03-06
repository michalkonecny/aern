{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduction
    Description :  size reduction of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Size reduction of interval polynomials.
-}

module Numeric.AERN.Poly.IntPoly.Reduction
    (
        reducePolyDegreeOut,
        reducePolyTermCountOut
    )
where
    
import Prelude hiding ((+),(*),(^))
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()

import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import Numeric.AERN.Basics.Consistency

import qualified Numeric.AERN.NumericOrder.OpsDefaultEffort as NumOrdDefEffort
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.PartialOrdering as PartialOrdering

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
import qualified Data.List as List

reducePolyTermCountOut ::
    (Show var,
     Show cf,
     HasDomainBox (IntPoly var cf),
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf) 
    =>
    ArithInOut.RoundedRealEffortIndicator cf -> 
    IntPoly var cf -> 
    IntPoly var cf
reducePolyTermCountOut effCf p =
    let ?addInOutEffort = effAdd in
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPow in
    reducePolyTermCountWithOps (<+>) (<*>) (<^>) (imprecisionOfEff effImpr) p
    where
    effAdd = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effMult = ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effPow = ArithInOut.fldEffortPow sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effImpr = ArithInOut.rrEffortImprecision sampleCf effCf
    sampleCf = getSampleDomValue p

reducePolyDegreeOut ::
    (Show var,
     Show cf,
     HasDomainBox (IntPoly var cf),
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf) 
    =>
    ArithInOut.RoundedRealEffortIndicator cf -> 
    IntPoly var cf -> 
    IntPoly var cf
reducePolyDegreeOut effCf p =
    let ?addInOutEffort = effAdd in
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPow in
    reducePolyDegreeWithOps (<+>) (<*>) (<^>) p
    where
    effAdd = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effMult = ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effPow = ArithInOut.fldEffortPow sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    sampleCf = getSampleDomValue p

reducePolyDegreeWithOps ::
    (Show var, Show cf, HasAntiConsistency cf) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (IntPoly var cf) ->
    (IntPoly var cf)
reducePolyDegreeWithOps (+) (*) (^) (IntPoly cfg terms) =
    IntPoly cfg $ reduceTermsDegreeWithOps (+) (*) (^) varDoms maxDeg terms
    where
    maxDeg = ipolycfg_maxdeg cfg
    varDoms = ipolycfg_domsLZ cfg
    
reducePolyTermCountWithOps ::
    (Show var, Show cf, 
     HasOne cf, HasAntiConsistency cf, 
     NumOrd.PartialComparison imprecision) =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) -> 
    (IntPoly var cf) ->
    (IntPoly var cf)
reducePolyTermCountWithOps (+) (*) (^) getImpr (IntPoly cfg terms) =
    IntPoly cfg $ reduceTermsCountWithOps (+) (*) (^) getImpr varDoms maxSize terms
    where
    maxSize = ipolycfg_maxsize cfg
    varDoms = ipolycfg_domsLZ cfg
    
reduceTermsDegreeWithOps ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) ->
    (cf -> cf -> cf) ->
    (cf -> Int -> cf) ->
    [cf] ->
    Int ->
    (IntPolyTerms var cf) ->
    (IntPolyTerms var cf)
reduceTermsDegreeWithOps (+) (*) (^) varDoms maxDeg terms
    =
--    (terms, result) `seq`
--    unsafePrintReturn
--    (
--        "reduceTermsDegree:"
--        ++ "\n maxDeg = " ++ show maxDeg
--        ++ "\n terms = " ++ show terms
--        ++ "\n termsMarkedExcessDegree = " ++ show termsMarkedExcessDegree
--        ++ "\n reducedTerms = "
--    )$
    result
    {- overview:
        * mark all terms whose degree exceeds maxDeg
        * remove the marked terms and compensate for them by widening the remaining coeffs
    -}
    where
    result = reduceMarkedTerms (+) (*) (^) varDoms termsMarkedExcessDegree
    termsMarkedExcessDegree = termsMapCoeffsWithDegrees markDegreeTooLarge terms 
    markDegreeTooLarge varDegrees coeff = (coeff, totalDegree > maxDeg)
        where
        totalDegree = sum varDegrees

reduceTermsCountWithOps ::
    (Show var, Show cf, 
     HasOne cf, NumOrd.PartialComparison imprecision) =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) -> 
    [cf] ->
    Int ->
    (IntPolyTerms var cf) ->
    (IntPolyTerms var cf)
reduceTermsCountWithOps (+) (*) (^) getImpr varDoms maxSize terms
    | size <= maxSize = terms
    | otherwise = 
--        unsafePrint
--        (
--            "reduceTermsCount:"
--            ++ "\n varDomsPowers = " ++ (show $ map (take 3) varDomsPowers)
--            ++ "\n allTermRangesListSortedDescending = " ++ show allTermRangesListSortedDescending  
--        ) $
        reduceMarkedTerms (+) (*) (^) varDoms termsMarkedTooSmall
    {- overview:
        * count terms, return without change if not over maxSize 
        * collect the widths of the ranges of all terms
        * sort the list and determine a cut-off treshold
        * (TODO) mark all terms whose range's width is below the treshold (but never mark the constant term!) 
        * remove the marked terms and compensate for them by widening the remaining coeffs
    -}
    where
    varDomsPowers = 
        map powersOf varDoms
        where
        powersOf a = iterate (* a) $ one a
    size = length allTermRangesList
    allTermRangesList = 
        termsCollectCoeffsWith evalTermRangeWidth terms
    evalTermRangeWidth varDegrees coeff =
        getImpr $ foldl (*) coeff $ zipWith (!!) varDomsPowers varDegrees
    tresholdSmallestAllowed = allTermRangesListSortedDescending !! (maxSize-1)
    allTermRangesListSortedDescending = List.sortBy compareCf allTermRangesList
        where
        compareCf a b = 
            case NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort a) a b of
                Just PartialOrdering.LT -> GT 
                Just PartialOrdering.GT -> LT 
                _ -> EQ 
    termsMarkedTooSmall =
        termsMapCoeffsWithDegrees markTooSmall terms
        where
        markTooSmall degrees cf = 
            (cf, tooSmall && notConst)
            where
            notConst = or $ map (/= 0) degrees
            tooSmall =
                (evalTermRangeWidth (reverse degrees) cf NumOrdDefEffort.<? tresholdSmallestAllowed) == Just True

reduceMarkedTerms ::
    (Show var, Show cf) =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    [cf] ->
    (IntPolyTerms var (cf,Bool)) ->
    (IntPolyTerms var cf)
reduceMarkedTerms (+) (*) (^) doms terms =
    case (maybeReducedTerms, maybeOverflow) of
        (Just reducedTerms, Nothing) -> reducedTerms
        _ -> error $ "internal error in reduceMarkedTerms: cannot reduce the constant term!\n  terms = " ++ show terms
    where  
    (maybeReducedTerms, maybeOverflow) = 
        aux doms terms

    aux _ (IntPolyC (cf, marked)) 
        | marked = (Nothing, Just $ IntPolyC (cf, True))
        | otherwise = (Just $ IntPolyC cf, Nothing)  
    aux (varDom:restVars) (IntPolyV var subPolys) =
        (maybeNewTerms, maybeOverflow2)
        where
        maybeNewTerms
            | null newTermsList = Nothing
            | otherwise = Just $ IntPolyV var $ IntMap.fromAscList newTermsList
        maybeOverflow2 =
            case (maybeLeastDegree, maybeSubOverflow) of
                (Just leastDegree, Just subOverflow) ->
                    Just $ IntPolyV var (IntMap.singleton leastDegree subOverflow)
                _ -> Nothing
        (maybeLeastDegree, maybeSubOverflow, newTermsList) 
            = IntMap.foldWithKey applyAux (Nothing, Nothing, []) subPolys
        applyAux degree subTerms (maybePrevDegree, maybePrevOverflowTerms, prevNewTerms)
            = 
            (Just degree, maybeNextOverflowTerms, nextNewTermsList)
            where
            nextNewTermsList = 
                case maybeNextNewTerms of
                    Nothing -> prevNewTerms
                    Just terms2 -> (degree, terms2) : prevNewTerms
            (maybeNextNewTerms, maybeNextOverflowTerms) 
                = aux restVars subTermsWithOverflow
            subTermsWithOverflow =
                case (maybePrevOverflowTerms, maybePrevDegree) of
                    (Nothing, _) -> subTerms
                    (Just prevOverflowTerms, Just prevDegree) ->
                        add subTerms (scaleByVarPower (prevDegree - degree) prevOverflowTerms)
        scaleByVarPower pwr p =
            (scale $ varDom ^ pwr) p
        scale c (IntPolyC (val, marked)) =
            IntPolyC (val * c, marked)
        scale c (IntPolyV x polys) = 
            IntPolyV x $ IntMap.map (scale c) polys
        
        add _poly1@(IntPolyC (val1, marked)) _poly2@(IntPolyC (val2, _)) 
            = IntPolyC $ (val1 + val2, marked) 
        add _poly1@(IntPolyV _xName1 polys1) _poly2@(IntPolyV xName2 polys2)
            = IntPolyV xName2 $ IntMap.unionWith add polys1 polys2
        {-
            Traverse subPolys from highest degree to lowest,
            applying aux.
            Whenever there are overflow terms, multiply them by
            the range of var raised to the difference in degrees.
            The add the scaled overflow either 
            to the following subpoly or return it if there is 
            no following subpoly.
        -}
    
