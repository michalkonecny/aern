{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Config
    Description :  configuration parameters of polynomial arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Configuration parameters of polynomial arithmetic.
-}

module Numeric.AERN.Poly.IntPoly.Config
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Domain (GeneratableVariables(..))

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

import Test.QuickCheck (Arbitrary, arbitrary, vectorOf)

data IntPolyCfg var cf =
    IntPolyCfg
    {
        ipolycfg_vars :: [var], -- arity and variable order
        ipolycfg_domsLZ :: [cf], -- domain of each variable - shifted so that the left endpoint is 0
        ipolycfg_domsLE :: [cf], -- the left endpoint of the domain for all variables
        ipolycfg_sample_cf :: cf,  -- sample coefficient (used only for type inference)
        ipolycfg_maxdeg :: Int, -- maximum degree of each term
        ipolycfg_maxsize :: Int -- maximum term count
    }

domToDomLZLE ::
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf)
    =>
    RefOrd.GetEndpointsEffortIndicator cf -> 
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    cf -> 
    (cf, cf)
domToDomLZLE effGetE effCF dom =
    (domLZ, domLE)
    where
    (domLE, _) = RefOrd.getEndpointsOutEff effGetE dom
    domLZ = 
        let ?addInOutEffort = effAdd in
        dom <-> domLE
    effAdd = 
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCF
    sampleCf = dom

domLZLEToDom ::
    (ArithInOut.RoundedReal cf)
    =>
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    cf -> 
    cf ->
    cf
domLZLEToDom effCF domLZ domLE =
    dom
    where
    dom = 
        let ?addInOutEffort = effAdd in
        domLZ <+> domLE
    effAdd = 
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCF
    sampleCf = dom
     
     
cfgRemVar :: IntPolyCfg var a -> IntPolyCfg var a
cfgRemVar cfg = cfg
        { 
            ipolycfg_vars = tail $ ipolycfg_vars cfg, 
            ipolycfg_domsLZ = tail $ ipolycfg_domsLZ cfg, 
            ipolycfg_domsLE = tail $ ipolycfg_domsLE cfg 
        }

instance 
    (Show var, Show cf, ArithInOut.RoundedReal cf) 
    =>
    Show (IntPolyCfg var cf)
    where
    show (IntPolyCfg vars domsLZ domsLE sampleCf maxdeg maxsize) 
        = 
        "cfg{" ++ (show $ zip vars doms) ++ ";" ++ show maxdeg ++ "/" ++ show maxsize ++ "}"
        where
        doms = zipWith (domLZLEToDom effCF) domsLZ domsLE
        effCF = ArithInOut.roundedRealDefaultEffort sampleCf

instance
    (RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison cf, 
     Arbitrary cf, GeneratableVariables var) 
    =>
    (Arbitrary (IntPolyCfg var cf))
    where
    arbitrary =
        do
        Int1To10 arity <- arbitrary
        Int1To10 maxdeg <- arbitrary
        Int1To1000 maxsizeRaw <- arbitrary
        sampleCfs <- vectorOf (50 * arity) arbitrary 
            -- probability that too many of these are anti-consistent is negligible
        efforts <- arbitrary
        return $ mkCfg arity maxdeg maxsizeRaw sampleCfs efforts
        where
        mkCfg arity maxdeg maxsizeRaw sampleCfs (effConsistency, effGetE, effCF) =
            IntPolyCfg
                vars domsLZ domsLE (head sampleCfs) maxdeg (max 2 maxsizeRaw)
            where
            vars = getNVariables arity
            (domsLZ, domsLE) = unzip $ map (domToDomLZLE effGetE effCF) doms
            doms = 
                take arity $ filter notAntiConsistent sampleCfs
                -- domain intervals must not be anti-contistent (in particular not singletons)
            notAntiConsistent a =
                (isAntiConsistentEff effConsistency a) == Just False
--                &&
--                nonnegative
--                where
--                nonnegative =
--                    case pNonnegNonposEff effNumComp a of
--                        (Just True,_) -> True
--                        _ -> False
             
instance
    (Show var, Show cf,
     RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf, 
     NumOrd.PartialComparison cf, 
     Arbitrary cf, GeneratableVariables var)
    =>
    (EffortIndicator (IntPolyCfg var cf))
    where
    effortIncrementVariants (IntPolyCfg vars domsLZ domsLE sample maxdeg maxsize) =
        map recreateCfg $ effortIncrementVariants (Int1To10 maxdeg, Int1To1000 maxsize)
        where
        recreateCfg (Int1To10 md, Int1To1000 ms) =
            IntPolyCfg vars domsLZ domsLE sample md ms 
    effortIncrementSequence (IntPolyCfg vars domsLZ domsLE sample maxdeg maxsize) =
        map recreateCfg $ effortIncrementSequence (Int1To10 maxdeg, Int1To1000 maxsize)
        where
        recreateCfg (Int1To10 md, Int1To1000 ms) =
            IntPolyCfg vars domsLZ domsLE sample md ms
    effortRepeatIncrement 
            (IntPolyCfg vars domsLZ domsLE sample maxdeg1 maxsize1, 
             IntPolyCfg _ _ _ _ maxdeg2 maxsize2)
        =
        IntPolyCfg vars domsLZ domsLE sample md ms
        where
        Int1To10 md = effortRepeatIncrement (Int1To10 maxdeg1, Int1To10 maxdeg2)  
        Int1To1000 ms = effortRepeatIncrement (Int1To1000 maxsize1, Int1To1000 maxsize2)  

