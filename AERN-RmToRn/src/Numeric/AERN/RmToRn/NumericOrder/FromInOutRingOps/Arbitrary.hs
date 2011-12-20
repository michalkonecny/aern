{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}
{-|
    Module      :  Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
    Description :  approximation of min and max using ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximation of min and max using ring operations.
    .
    The motivating use case for this module is where we compute min or max for a 
    /function/ pointwise over its domain.
-}

module Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort
----import Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort

--import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort

--import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Consistency

import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Numeric.AERN.Misc.QuickCheck
import qualified System.Random as R


type Area4FunFromRingOps f = 
    (f, Maybe ConsistencyStatus, Maybe (Domain f))

areaWhole4FunFromRingOps sampleFn =
    (sampleFn, Nothing, Nothing)
    
arbitraryTupleInAreaRelatedBy4FunFromRingOps ::
    (Show ix, Ord ix, 
     HasDomainBox fn,  HasConstFns fn, HasProjections fn, 
     ArithInOut.RoundedAdd fn,
     ArithInOut.RoundedMultiply fn,
     HasOne (Domain fn), HasZero (Domain fn), ArithInOut.RoundedSubtr (Domain fn),
     RefOrd.ArbitraryOrderedTuple (Domain fn),
     ArithUpDn.Convertible fn (Domain fn),
     ArithInOut.RoundedMixedAdd fn (Domain fn),
     ArithInOut.RoundedMixedMultiply fn (Domain fn)
    )
    =>
    (ArithInOut.AddEffortIndicator (Domain fn),
     (ArithInOut.AddEffortIndicator fn,
      ArithInOut.MultEffortIndicator fn),
     (ArithInOut.MixedAddEffortIndicator fn (Domain fn),
      ArithInOut.MixedMultEffortIndicator fn (Domain fn)
     )
    ) ->
    (Area4FunFromRingOps fn) ->
    [ix] -> 
    [((ix, ix),[PartialOrdering])]
    -> Maybe (Gen [fn])
arbitraryTupleInAreaRelatedBy4FunFromRingOps 
        (effAddDom, 
         (effAddFn, effMultFn), 
         (effAddFnDom, effMultFnDFn)) 
        area@(sampleFn, maybeCons, maybeRange) =
    let ?addInOutEffort = effAddDom in
    let ?mixedAddInOutEffort = effAddFnDom in
    NumOrd.forcedLinearArbitraryTupleRelatedBy 
        (arbitraryFromSequence (fixedSeq $ arbitraryFn (effAddFn, effMultFn, effMultFnDFn) sampleFn)) 
        pickAndShiftGetSorted
    where
    arbitraryFromSequence seq 
        =
        sized $ \size ->
        do
        ix <- choose (0, quantityOfSize size - 1)
        return $ 
--            unsafePrintReturn ("arbitraryTupleInAreaRelatedBy4FunFromRingOps: size = " ++ show size ++ "ix = " ++ show ix ++ " p = ") $ 
                seq !! ix
    pickAndShiftGetSorted seed n list 
        | n < 1 = []
        | otherwise 
            =
            pick n first (zip randomBools rest)
            where
            pick n (fnPrev, lowerPrev, upperPrev) ((copyPrev, current@(fn,lower,upper)):rest) 
                | n == 0 = []
                | copyPrev
                    =
                    fnPrevTrCp : (pick (n-1) (fnPrevTrCp, lowerPrev, upperPrevTrCp) ((False, current):rest))
                | otherwise
                    =
                    fnTr : (pick (n-1) (fnTr, lowerPrev, upperPrevTr) rest) 
                where
                fnPrevTrCp = fnPrev <+>| distCp
                upperPrevTrCp = upperPrev <+> distCp
                distCp = one sampleDom
                fnTr = fn <+>| dist
                upperPrevTr = upperPrev <+> dist
                dist = (upperPrev <-> lower) <+> (one sampleDom) 
                sampleDom = snd $ head $ toAscList $ getDomainBox fn
                
            fnBounds@(first:rest) = map addBounds list
            addBounds fn = (fn, lower, upper)
                where
                Just upper = ArithUpDn.convertUpEff eff fn
                Just lower = ArithUpDn.convertDnEff eff fn
                eff = ArithUpDn.convertDefaultEffort fn z
                z = zero sampleDom
                sampleDom = getSampleDomValue fn
                _ = [z, upper, lower]
            randomBools = 
                map even $ 
                    map fst $ 
                        drop 13 $ iterate (R.next . snd) (0,g)
            g = R.mkStdGen seed

arbitraryFn ::
    (HasProjections fn, HasConstFns fn,
     ArithInOut.RoundedAdd fn,
     ArithInOut.RoundedMultiply fn,
     ArithInOut.RoundedMixedMultiply fn (Domain fn),
     RefOrd.ArbitraryOrderedTuple (Domain fn),
     HasOne (Domain fn)
    )
    =>
    (ArithInOut.AddEffortIndicator fn, 
     ArithInOut.MultEffortIndicator fn,
     ArithInOut.MixedMultEffortIndicator fn (Domain fn)) ->
    fn ->
    Gen fn
arbitraryFn 
        (effAddFn, effMultFn, effMultFnDFn) 
        sampleFn
    =
    let ?multInOutEffort = effMultFn in
    let ?addInOutEffort = effAddFn in
    let ?mixedMultInOutEffort = effMultFnDFn in
    sized $ \size ->
    do
    -- choose polynomial generation parameters:
    degree <- choose (0,2 + (size `div` 7))
    constrTerms <- choose (1,3 + 2* size)
    -- start building the result polynomial, first creating variables:
    varFns <- mapM (\v -> return $ newProjection sizeLimits box v) vars
    -- now multiplying variables in various powers:  
    powerTerms <- mapM (const $ arbitraryPowerTerm varFns degree) [1..constrTerms]
    -- and combining them as a linear combination:
    coeffsL <- vectorOf constrTerms ((\(Just a) -> a) $ RefOrd.arbitraryTuple 1)
    let coeffs = map (\[a] -> a) coeffsL
    let _ = sampleDom : coeffs
    return $ 
        foldl1 (<+>) $ zipWith (<*>|) powerTerms coeffs
    where
    arbitraryPowerTerm varFns degree
        | null varFns || degree == 0 
            = return $ newConstFn sizeLimits box $ one sampleDom
        | otherwise = 
            do
            (n,varFn) <- elements $ zip [0..] varFns
            dg <- choose (1,degree)
            let remainingVarFns = take (n-1) varFns ++ drop (n+1) varFns
            restFn <- arbitraryPowerTerm remainingVarFns (degree - dg)
            let varFnPwr = foldl1 (<*>) $ replicate dg varFn 
            return $ varFnPwr <*> restFn -- rounding direction irrelevant
    sizeLimits = getSizeLimits sampleFn
    sampleDom = getSampleDomValue sampleFn
    box = getDomainBox sampleFn
    vars = getVars box
        


{-|
  Have a fairly long and hairy sequence of elements of increasing complexity
  pre-generated and fixed and then pick from it randomly.
  This deals with the problem that the random generation takes a long time
  when the elements' construction is expensive, eg when functions are built
  using a fairly large sequence of multiplications and additions.
-}
fixedSeq ::
    Gen a -> [a]
fixedSeq gen =
    aux 0 0
    where
    aux prevQuantity size 
        = newSeqPortion ++ (aux currQuantity (size + 1))
        where
        newSeqPortion 
            =
            take (currQuantity - prevQuantity) $ 
                map (\g -> unGen gen g size) randomGens
        currQuantity = quantityOfSize size
    randomGens 
        = map snd $ drop 13 $ iterate (R.next . snd) (0,g)
    g = R.mkStdGen 754657854089 -- no magic, just bashed at the keyboard at random
quantityOfSize size
    = 10 + ((size*(size+100)) `div` 10)  


