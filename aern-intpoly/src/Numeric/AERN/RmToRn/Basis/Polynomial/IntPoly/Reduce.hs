{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce
    Description :  size reduction of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Size reduction of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics

--import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import Numeric.AERN.Basics.Consistency

import qualified Numeric.AERN.NumericOrder.OpsDefaultEffort as NumOrdDefEffort
import Numeric.AERN.RefinementOrder.OpsImplicitEffort
import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.PartialOrdering as PartialOrdering

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
import qualified Data.List as List

reducePolyDegree ::
    (Show var, Show cf, HasAntiConsistency cf) 
    =>
    Bool ->
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (IntPoly var cf) ->
    (IntPoly var cf)
reducePolyDegree isOut (+) (*) (^) p@(IntPoly cfg terms) =
    IntPoly cfg $ reduceTermsDegree (+) (*) (^) varDoms maxDeg terms
    where
    maxDeg = ipolycfg_maxdeg cfg
    varDoms
        | isOut = ipolycfg_domsLZ cfg
        | otherwise = map flipConsistency $ ipolycfg_domsLZ cfg
    
reducePolyTermCount ::
    (Show var, Show cf, 
     HasOne cf, HasAntiConsistency cf, 
     NumOrd.PartialComparison imprecision) =>
    Bool ->
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) -> 
    (IntPoly var cf) ->
    (IntPoly var cf)
reducePolyTermCount isOut (+) (*) (^) getImpr p@(IntPoly cfg terms) =
    IntPoly cfg $ reduceTermsCount (+) (*) (^) getImpr varDoms maxSize terms
    where
    maxSize = ipolycfg_maxsize cfg
    varDoms
        | isOut = ipolycfg_domsLZ cfg
        | otherwise = map flipConsistency $ ipolycfg_domsLZ cfg
    
reduceTermsDegree ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) ->
    (cf -> cf -> cf) ->
    (cf -> Int -> cf) ->
    [cf] ->
    Int ->
    (IntPolyTerms var cf) ->
    (IntPolyTerms var cf)
reduceTermsDegree (+) (*) (^) varDoms maxDeg terms
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
    termsMarkedExcessDegree = mapTermsCoeffsWithDegrees markDegreeTooLarge terms 
    markDegreeTooLarge varDegrees coeff = (coeff, totalDegree > maxDeg)
        where
        totalDegree = sum varDegrees

reduceTermsCount ::
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
reduceTermsCount (+) (*) (^) getImpr varDoms maxSize terms
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
        (collectCoeffs evalTermRangeWidth) terms
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
        mapTermsCoeffsWithDegrees markTooSmall terms
        where
        markTooSmall degrees cf = 
            (cf, tooSmall && notConst)
            where
            notConst = or $ map (/= 0) degrees
            tooSmall =
                (evalTermRangeWidth (reverse degrees) cf NumOrdDefEffort.<? tresholdSmallestAllowed) == Just True

collectCoeffs fn terms = aux [] terms
    where
    aux prevDegrees (IntPolyC cf) = [fn (reverse prevDegrees) cf] 
    aux prevDegrees (IntPolyV x polys) = 
        IntMap.fold (++) [] $ IntMap.mapWithKey applyAux polys
        where
        applyAux degree = aux (degree : prevDegrees)

--countTermsCoeffsSatisfying cond (IntPolyC cf) 
--    | cond cf = 1
--    | otherwise = 0
--countTermsCoeffsSatisfying cond (IntPolyV x polys) = 
--    IntMap.fold (+) 0 $ IntMap.map (countTermsCoeffsSatisfying cond) polys

mapTermsCoeffsWithDegrees ::
    ([Int] -> cf1 -> cf2) {-^ mapping function whose first argument is the list of degrees for each variable in _reverse_ order -} ->
    (IntPolyTerms var cf1) ->
    (IntPolyTerms var cf2)
mapTermsCoeffsWithDegrees fn terms = aux [] terms    
    where
    aux prevDegrees (IntPolyC cf) = IntPolyC $ fn prevDegrees cf
    aux prevDegrees (IntPolyV x polys) =
        IntPolyV x $ IntMap.mapWithKey applyAux polys
        where
        applyAux degree = aux (degree : prevDegrees)
        
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

    aux _ t@(IntPolyC (cf, marked)) 
        | marked = (Nothing, Just $ IntPolyC (cf, True))
        | otherwise = (Just $ IntPolyC cf, Nothing)  
    aux (varDom:restVars) (IntPolyV var subPolys) =
        (maybeNewTerms, maybeOverflow)
        where
        maybeNewTerms
            | null newTermsList = Nothing
            | otherwise = Just $ IntPolyV var $ IntMap.fromAscList newTermsList
        maybeOverflow =
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
                    Just terms -> (degree, terms) : prevNewTerms
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
        
        add poly1@(IntPolyC (val1, marked)) poly2@(IntPolyC (val2, _)) 
            = IntPolyC $ (val1 + val2, marked) 
        add poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2)
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
    
