{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.MinimalFnBasis.NumericOrder
    Description :  comparing fucntions and generating random ones
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Comparing fucntions and generating random ones.
-}

module Numeric.AERN.RmToRn.MinimalFnBasis.NumericOrder where

import Numeric.AERN.RmToRn.MinimalFnBasis.Basics
import Numeric.AERN.RmToRn.MinimalFnBasis.RingOps

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.Interval (Interval(..))
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Numeric.AERN.Misc.QuickCheck
import qualified System.Random as R

import Numeric.AERN.Misc.Debug

import Control.Monad (zipWithM)

instance (MinimalFnBasis fb
    , Show fb, Show (Domain fb)
    ) 
    => NumOrd.PartialComparison (FnEndpoint fb)
    where
    type NumOrd.PartialCompareEffortIndicator (FnEndpoint fb) =
        (ArithUpDn.AddEffortIndicator fb, 
         ArithUpDn.ConvertEffortIndicator fb (Domain fb),
         NumOrd.PartialCompareEffortIndicator (Domain fb))
    pCompareDefaultEffort (FnEndpoint f) =
        (ArithUpDn.addDefaultEffort f,
         ArithUpDn.convertDefaultEffort f (getSampleDomValue f),
         NumOrd.pCompareDefaultEffort (getSampleDomValue f))
    pCompareEff (effAdd, effBnd, effCompDom) f1 f2 =
--        unsafePrint
--        (
--            "FnEndpoint NumOrd.pCompareEff:"
--            ++ "\n f1 = " ++ show f1
--            ++ "\n f2 = " ++ show f2
--            ++ "\n diffUp = " ++ show diffUp
--            ++ "\n diffDn = " ++ show diffDn
--            ++ "\n bUp = " ++ show bUp
--            ++ "\n bDn = " ++ show bDn
--        ) $
        NumOrd.pCompareEff effCompDom (Interval bDn bUp) zero
        where
        _ = [bUp, getSampleDomValue f1]
        Just bUp = ArithUpDn.convertUpEff effBnd diffUp
        Just bDn = ArithUpDn.convertDnEff effBnd diffDn
        diffUp = ArithUpDn.subtrUpEff effAdd f1 f2 
        diffDn = ArithUpDn.subtrDnEff effAdd f1 f2 
        

instance 
    (MinimalFnBasis fb) => Arbitrary (FnEndpoint fb)
    where
    arbitrary
        =
        sized $ \size ->
        do
        sizeLimits <- arbitrary
        maxArity <- choose (1, 2 + (size `div` 4))
        arbitraryFn maxArity sizeLimits

arbitraryFn maxArity sizeLimits
    =
    sized $ \size ->
    do
    -- extract domain box and samples from sizeLimits:
    let
        box = fromAscList $ zip vars (repeat $ fixedDomain sampleFn)
        (sampleVar,Interval sampleDom _) = head $ toAscList box
        sampleFn = newProjection sizeLimits box sampleVar
        vars = getNVariables sampleFn maxArity
    -- choose polynomial generation parameters:
    arity <- choose (1, maxArity)
    degree <- choose (0,2 + (size `div` 7))
    constrTerms <- choose (1,3 + 2* size)
    -- start building the result polynomial, first creating variables:
    varFns <- mapM (\v -> return $ newProjection sizeLimits box v) vars
    let _ = sampleFn : varFns
    -- now multiplying variables in various powers:  
    powerTerms <- mapM (const $ arbitraryPowerTerm box varFns degree) [1..constrTerms]
    -- and combining them as a linear combination:
    coeffsL <- vectorOf constrTerms ((\(Just a) -> a) $ NumOrd.arbitraryTuple 1)
    let coeffs = map (\[a] -> a) coeffsL
    let _ = sampleDom : coeffs
    return $ 
        foldl1 (+^) $ zipWith (*^|) powerTerms coeffs
    where
    arbitraryPowerTerm box varFns degree
        | null varFns || degree == 0 
            = return $ newConstFn sizeLimits box one
        | otherwise = 
            do
            (n,varFn) <- elements $ zip [0..] varFns
            dg <- choose (1,degree)
            let remainingVarFns = take (n-1) varFns ++ drop (n+1) varFns
            restFn <- arbitraryPowerTerm box remainingVarFns (degree - dg)
            let varFnPwr = foldl1 (*.) $ replicate dg varFn 
            return $ varFnPwr *^ restFn -- rounding direction irrelevant 
        
{-|
  Have a fairly long and hairy sequence of polynomials of increasing complexity
  pre-generated and fixed and then pick its elements randomly.
  This deals with the problem that the random generation takes a long time
  when the polynomial is constructed by a fairly large sequence of
  multiplications and additions.
-}
fixedSeq ::
    (Arbitrary fb) => [fb]
fixedSeq =
    aux 0 0
    where
    aux prevQuantity size 
        = newSeqPortion ++ (aux currQuantity (size + 1))
        where
        newSeqPortion 
            =
            take (currQuantity - prevQuantity) $ 
                map (\g -> unGen arbitrary g size) randomGens
        currQuantity = quantityOfSize size
    randomGens 
        = map snd $ drop 13 $ iterate (R.next . snd) (0,g)
    g = R.mkStdGen 754657854089 -- no magic, just bashed at the keyboard at random
            
quantityOfSize size
    = 10 + ((size*(size+100)) `div` 10)  
            
instance (MinimalFnBasis fb, Show fb) => NumOrd.ArbitraryOrderedTuple (FnEndpoint fb)
    where
    type (NumOrd.Area (FnEndpoint fb)) = () 
        -- TODO: make it possible to guide the generator to generate
        --       functions whose range is within a given interval,
        --       whose generating polynomial is of a certain degree range,
        --       whose generating polynomial coeffs are in a certain area...
    areaWhole _ = ()
    arbitraryTupleInAreaRelatedBy _
        =
        NumOrd.forcedLinearArbitraryTupleRelatedBy (arbitraryFromSequence fixedSeq) pickAndShiftGetSorted
        where
        arbitraryFromSequence seq 
            =
            sized $ \size ->
            do
            ix <- choose (0, quantityOfSize size - 1)
            return $ 
--                unsafePrintReturn ("FnEndpoint: ArbitraryOrderedTuple: arbitraryFromSequence: size = " ++ show size ++ "ix = " ++ show ix ++ " p = ") $ 
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
                    fnPrevTrCp = fnPrev +.| distCp
                    upperPrevTrCp = upperPrev +^ distCp
                    distCp = one
                    fnTr = fn +.| dist
                    upperPrevTr = upperPrev +^ dist
                    dist = (upperPrev -^ lower) +^ one 
                    
                fnBounds@(first:rest) = map addBounds list
                addBounds fn = (fn, lower, upper)
                    where
                    Just upper = ArithUpDn.convertUpEff eff fn
                    Just lower = ArithUpDn.convertDnEff eff fn
                    eff = ArithUpDn.convertDefaultEffort fn z
                    z = zero
                    Interval sampleDom _ = snd $ head $ toAscList $ getDomainBox fn
                    _ = [sampleDom, z, upper, lower]
                randomBools = 
                    map even $ 
                        map fst $ 
                            drop 13 $ iterate (R.next . snd) (0,g)
                g = R.mkStdGen seed
    arbitraryTupleRelatedBy =
        NumOrd.arbitraryTupleInAreaRelatedBy ()
        