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

import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.NumericOrder.OpsDefaultEffort as NumOrdDefEffort
import Numeric.AERN.RefinementOrder.OpsImplicitEffort
import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.PartialOrdering as PartialOrdering

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
import qualified Data.List as List

reducePolyDegree eff p@(IntPoly cfg terms) =
    IntPoly cfg $ reduceTermsDegree eff cfg terms
    
reducePolyTermCount eff p@(IntPoly cfg terms) =
    IntPoly cfg $ reduceTermsCount eff cfg terms
    
reduceTermsDegree ::
    (ArithInOut.RoundedReal cf, Show var, Show cf) =>
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    (IntPolyCfg var cf) ->
    (IntPolyTerms var cf) ->
    (IntPolyTerms var cf)
reduceTermsDegree eff cfg terms
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
    result = reduceMarkedTerms eff cfg termsMarkedExcessDegree
    maxDeg = ipolycfg_maxdeg cfg
    termsMarkedExcessDegree = mapTermsCoeffsWithDegrees markDegreeTooLarge terms 
    markDegreeTooLarge varDegrees coeff = (coeff, totalDegree > maxDeg)
        where
        totalDegree = sum varDegrees

reduceTermsCount ::
    (ArithInOut.RoundedReal cf, Show var, Show cf, 
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf)) =>
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    (IntPolyCfg var cf) ->
    (IntPolyTerms var cf) ->
    (IntPolyTerms var cf)
reduceTermsCount eff cfg terms
    | size <= maxSize = terms
    | otherwise = 
--        unsafePrint
--        (
--            "reduceTermsCount:"
--            ++ "\n varDomsPowers = " ++ (show $ map (take 3) varDomsPowers)
--            ++ "\n allTermRangesListSortedDescending = " ++ show allTermRangesListSortedDescending  
--        ) $
        reduceMarkedTerms eff cfg termsMarkedTooSmall
    {- overview:
        * count terms, return without change if not over maxSize 
        * collect the widths of the ranges of all terms
        * sort the list and determine a cut-off treshold
        * (TODO) mark all terms whose range's width is below the treshold (but never mark the constant term!) 
        * remove the marked terms and compensate for them by widening the remaining coeffs
    -}
    where
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff
    effImprecision = ArithInOut.rrEffortImprecision sample eff
    sample = ipolycfg_sample_cf cfg

    varDoms = ipolycfg_domsLZ cfg
    varDomsPowers = 
        let ?joinmeetEffort = effJoin in
        let ?multInOutEffort = effMult in 
        map powersOf varDoms
        where
        powersOf a = iterate (<*> a) $ one a
    maxSize = ipolycfg_maxsize cfg
    size = length allTermRangesList
    allTermRangesList = 
        let ?multInOutEffort = effMult in 
        (collectCoeffs evalTermRangeWidth) terms
    evalTermRangeWidth varDegrees coeff =
        imprecisionOfEff effImprecision $ foldl (<*>) coeff $ zipWith (!!) varDomsPowers varDegrees
    tresholdSmallestAllowed = allTermRangesListSortedDescending !! (maxSize-1)
    allTermRangesListSortedDescending = List.sortBy compareCf allTermRangesList
        where
        compareCf a b = 
            case NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort a) a b of
                Just PartialOrdering.LT -> GT 
                Just PartialOrdering.GT -> LT 
--                Just PartialOrdering.LEE -> GT 
--                Just PartialOrdering.GEE -> LT 
                _ -> EQ 
    termsMarkedTooSmall =
        let ?multInOutEffort = effMult in
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
    (ArithInOut.RoundedReal cf, Show var, Show cf) =>
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    (IntPolyCfg var cf) ->
    (IntPolyTerms var (cf,Bool)) ->
    (IntPolyTerms var cf)
reduceMarkedTerms eff cfg terms =
    case (maybeReducedTerms, maybeOverflow) of
        (Just reducedTerms, Nothing) -> reducedTerms
        _ -> error $ "internal error in reduceMarkedTerms: cannot reduce the constant term!\n  terms = " ++ show terms
    where  
    (maybeReducedTerms, maybeOverflow) = 
        let ?multInOutEffort = effMult in
        let ?intPowerInOutEffort = effPwr in
        let ?addInOutEffort = effAdd in
        let ?joinmeetEffort = effJoin in
        aux doms terms
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff
    sample = ipolycfg_sample_cf cfg

    doms = ipolycfg_domsLZ cfg
    
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
            (scale $ varDom <^> pwr) p
        scale c (IntPolyC (val, marked)) =
            IntPolyC (val <*> c, marked)
        scale c (IntPolyV x polys) = 
            IntPolyV x $ IntMap.map (scale c) polys
        
        add poly1@(IntPolyC (val1, marked)) poly2@(IntPolyC (val2, _)) 
            = IntPolyC $ (val1 <+> val2, marked) 
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
    
