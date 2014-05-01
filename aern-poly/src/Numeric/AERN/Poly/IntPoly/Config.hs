{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Config
    Description :  Extra data kept with each interval polynomial.
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Extra data kept with each interval polynomial.
-}

module Numeric.AERN.Poly.IntPoly.Config
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Domain (GeneratableVariables(..))

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.Operators

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.Operators

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.SizeLimits
import Numeric.AERN.Basics.Consistency

import Test.QuickCheck (Arbitrary, arbitrary, vectorOf)
import Data.List (elemIndex)

data IntPolyCfg var cf =
    IntPolyCfg
    {
        ipolycfg_vars :: [var], -- arity and variable order
        ipolycfg_domsLZ :: [cf], -- domain of each variable - shifted so that the left endpoint is 0
        ipolycfg_domsLE :: [cf], -- the left endpoint of the domain for all variables
        ipolycfg_sample_cf :: cf,  -- sample coefficient (used only for type inference)
        ipolycfg_limits :: IntPolySizeLimits cf,
        ipolycfg_effort :: IntPolyEffort cf 
    }

defaultIntPolyCfg ::
    (RefOrd.IntervalLike cf, ArithInOut.RoundedReal cf) 
    => 
    cf -> IntPolySizeLimits cf -> IntPolyCfg var cf
defaultIntPolyCfg sampleCf limits =
    IntPolyCfg [] [] [] sampleCf limits (defaultIntPolyEffort sampleCf 2 limits)

data IntPolySizeLimits cf =
    IntPolySizeLimits
    {
        ipolylimits_cf_limits :: SizeLimits cf,
        ipolylimits_maxdeg :: Int, -- maximum degree of each term
        ipolylimits_maxsize :: Int -- maximum term count
    }

defaultIntPolySizeLimits :: SizeLimits cf -> IntPolySizeLimits cf
defaultIntPolySizeLimits cf_limist =
    IntPolySizeLimits cf_limist default_maxdeg default_maxsize

default_maxdeg :: Int
default_maxdeg = 3
default_maxsize :: Int
default_maxsize = 50

ipolycfg_maxdeg :: IntPolyCfg var cf -> Int
ipolycfg_maxdeg = ipolylimits_maxdeg . ipolycfg_limits

ipolycfg_maxsize :: IntPolyCfg var cf -> Int
ipolycfg_maxsize = ipolylimits_maxsize . ipolycfg_limits

data IntPolyEffort cf =
    IntPolyEffort
    {
        ipolyeff_cfRoundedRealEffort :: ArithInOut.RoundedRealEffortIndicator cf,
        ipolyeff_cfAbsEffort :: ArithInOut.AbsEffortIndicator cf,
        ipolyeff_cfGetEndpointsEffort :: RefOrd.GetEndpointsEffortIndicator cf,
        ipolyeff_cfFromEndpointsEffort :: RefOrd.FromEndpointsEffortIndicator cf,
        ipolyeff_cfMinMaxEffort :: NumOrd.MinmaxInOutEffortIndicator cf,
        ipolyeff_evalMaxSplitSize :: Int1To100,
        ipolyeff_minmaxBernsteinDegreeMinus1 :: Int1To10,
        ipolyeff_recipTauDegreeMinus1 :: Int1To10,
        ipolyeff_counterExampleSearchSampleCount :: Int1To1000
    }
--    deriving (Show)
    
-- TODO: complete the following instances:
instance Arbitrary (IntPolyEffort cf)
instance Show (IntPolyEffort cf)
instance EffortIndicator (IntPolyEffort cf)
    
defaultIntPolyEffort :: 
    (RefOrd.IntervalLike cf, ArithInOut.RoundedReal cf) 
    =>
    cf -> Int -> IntPolySizeLimits cf -> IntPolyEffort cf
defaultIntPolyEffort sampleCf arity sizeLimits =
    IntPolyEffort
    {
        ipolyeff_cfRoundedRealEffort = ArithInOut.roundedRealDefaultEffort sampleCf,
        ipolyeff_cfAbsEffort = ArithInOut.absDefaultEffort sampleCf,
        ipolyeff_cfGetEndpointsEffort = RefOrd.getEndpointsDefaultEffort sampleCf,
        ipolyeff_cfFromEndpointsEffort = RefOrd.fromEndpointsDefaultEffort sampleCf,
        ipolyeff_cfMinMaxEffort = NumOrd.minmaxInOutDefaultEffort sampleCf,
        ipolyeff_evalMaxSplitSize = Int1To100 maxSplitSize,
        ipolyeff_minmaxBernsteinDegreeMinus1 = Int1To10 (bernsteinDegree - 1),
        ipolyeff_recipTauDegreeMinus1 = Int1To10 (tauDegree - 1),
        ipolyeff_counterExampleSearchSampleCount = Int1To1000 (4 * arity)
    }
    where
    tauDegree = 2 + (min 20 $ maxdeg `div` 3)
    bernsteinDegree = 2 + (min 20 $ maxdeg `div` 3)
         -- TODO: the minimum 20 makes sense only with Double coeffs;
         --       make it depend on the current coefficient precision
    maxSplitSize = (1 + (maxdeg  `div` 2)) 
    maxdeg = ipolylimits_maxdeg sizeLimits

domToDomLZLEEff ::
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf)
    =>
    RefOrd.GetEndpointsEffortIndicator cf -> 
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    cf -> 
    (cf, cf)
domToDomLZLEEff effGetE effCF dom =
    (domLZ, domLE)
    where
    (domLE, _) = RefOrd.getEndpointsOutEff effGetE dom
    domLZ = dom <-> domLE
        
    (<->) = ArithInOut.subtrOutEff effAdd
    effAdd = 
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCF
    sampleCf = dom

domToDomLZLE ::
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf)
    =>
    cf -> 
    (cf, cf)
domToDomLZLE dom =
    domToDomLZLEEff effGetE effCF dom
    where
    effCF = ArithInOut.roundedRealDefaultEffort dom
    effGetE = RefOrd.getEndpointsDefaultEffort dom 

domLZLEToDomEff ::
    (ArithInOut.RoundedReal cf)
    =>
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    cf -> 
    cf ->
    cf
domLZLEToDomEff effCF domLZ domLE =
    dom
    where
    dom = domLZ <+> domLE

    (<+>) = ArithInOut.addOutEff effAdd
    effAdd = 
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCF
    sampleCf = dom

domLZLEToDom ::
    (ArithInOut.RoundedReal cf)
    =>
    cf -> 
    cf ->
    cf
domLZLEToDom domLZ domLE =
    domLZLEToDomEff effCF domLZ domLE
    where
    effCF = ArithInOut.roundedRealDefaultEffort domLZ
     
     
cfgRemFirstVar :: IntPolyCfg var a -> IntPolyCfg var a
cfgRemFirstVar cfg = 
    cfg
    { 
        ipolycfg_vars = tail $ ipolycfg_vars cfg, 
        ipolycfg_domsLZ = tail $ ipolycfg_domsLZ cfg, 
        ipolycfg_domsLE = tail $ ipolycfg_domsLE cfg 
    }

cfgRemVar ::
    (Eq var)
    => 
    var -> IntPolyCfg var a -> IntPolyCfg var a
cfgRemVar var cfg = 
    cfg
    { 
        ipolycfg_vars = dropAtVarPos $ ipolycfg_vars cfg, 
        ipolycfg_domsLZ = dropAtVarPos $ ipolycfg_domsLZ cfg, 
        ipolycfg_domsLE = dropAtVarPos $ ipolycfg_domsLE cfg 
    }
    where
    dropAtVarPos :: [a] -> [a]
    dropAtVarPos list = 
        take varPos list ++ drop (varPos + 1) list
    varPos = 
        case elemIndex var vars of
            Just pos -> pos
            Nothing -> error $ "aern-poly: cfgRemVar: var not in cfg"
    vars = ipolycfg_vars cfg

cfgAdjustDomains ::
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    [var] -> [cf] -> IntPolyCfg var cf -> IntPolyCfg var cf
cfgAdjustDomains vars newDomains cfg = 
    cfg
    { 
        ipolycfg_vars = vars,
        ipolycfg_domsLZ = newDomsLZ, 
        ipolycfg_domsLE = newDomsLE 
    }
    where
    (newDomsLZ, newDomsLE)
        = unzip $ map domToDomLZLE newDomains
   
cfg2vardomains :: 
     ArithInOut.RoundedReal cf 
     =>
     IntPolyCfg var cf 
     -> 
     [(var, cf)]
cfg2vardomains cfg =
    zip vars domains
    where
    vars = ipolycfg_vars cfg
    domsLZ = ipolycfg_domsLZ cfg
    domsLE = ipolycfg_domsLE cfg
    domains =
        zipWith domLZLEToDom domsLZ domsLE
    
instance 
    (Show var, Show cf, ArithInOut.RoundedReal cf, Show (SizeLimits cf)) 
    =>
    Show (IntPolyCfg var cf)
    where
    show (IntPolyCfg vars domsLZ domsLE sampleCf limits _effort) -- TODO: show also effort 
        = 
        "cfg{" ++ (show $ zip vars doms) ++ ";" ++ show limits ++ "}"
        where
        doms = zipWith (domLZLEToDomEff effCF) domsLZ domsLE
        effCF = ArithInOut.roundedRealDefaultEffort sampleCf

instance 
    (Show (SizeLimits cf)) 
    =>
    Show (IntPolySizeLimits cf)
    where
    show (IntPolySizeLimits cfLimits maxdeg maxsize) =
         "cf:" ++ show cfLimits ++ "/dg:" ++ show maxdeg ++ "/sz:" ++ show maxsize

instance
    (RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison cf, 
     Arbitrary cf, Arbitrary (SizeLimits cf), GeneratableVariables var) 
    =>
    (Arbitrary (IntPolyCfg var cf))
    where
    arbitrary =
        do
        Int1To10 arity <- arbitrary
        limits <- arbitrary
        sampleCfs <- vectorOf (50 * arity) arbitrary 
            -- probability that too many of these are anti-consistent is negligible
        efforts <- arbitrary
        return $ mkCfg arity limits sampleCfs efforts
        where
        mkCfg arity limits sampleCfs (effConsistency, effGetE, effCF) =
            IntPolyCfg
                vars domsLZ domsLE sampleCf limits (defaultIntPolyEffort sampleCf arity limits)
            where
            sampleCf = head sampleCfs
            vars = getNVariables arity
            (domsLZ, domsLE) = unzip $ map (domToDomLZLEEff effGetE effCF) doms
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
    (Arbitrary (SizeLimits cf)) 
    =>
    (Arbitrary (IntPolySizeLimits cf))
    where
    arbitrary =
        do
        cfLimits <- arbitrary
        Int1To10 maxdeg <- arbitrary
        Int1To1000 maxsizeRaw <- arbitrary
        return $ IntPolySizeLimits cfLimits maxdeg (max 2 maxsizeRaw)

instance
    (Show cf,
     RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf, 
     NumOrd.PartialComparison cf,
     EffortIndicator (SizeLimits cf), 
     Arbitrary cf)
    =>
    (EffortIndicator (IntPolySizeLimits cf))
    where
    effortIncrementVariants (IntPolySizeLimits cfLimits maxdeg maxsize) =
        map recreateLimits $ effortIncrementVariants (Int1To10 maxdeg, Int1To1000 maxsize)
        where
        recreateLimits (Int1To10 md, Int1To1000 ms) =
            IntPolySizeLimits cfLimits md ms 
    effortIncrementSequence (IntPolySizeLimits cfLimits maxdeg maxsize) =
        map recreateLimits $ effortIncrementSequence (Int1To10 maxdeg, Int1To1000 maxsize)
        where
        recreateLimits (Int1To10 md, Int1To1000 ms) =
            IntPolySizeLimits cfLimits md ms 
    effortRepeatIncrement 
            (IntPolySizeLimits cfLimits1 maxdeg1 maxsize1, 
             IntPolySizeLimits cfLimits2 maxdeg2 maxsize2)
        =
        IntPolySizeLimits (effortRepeatIncrement (cfLimits1, cfLimits2)) md ms
        where
        Int1To10 md = effortRepeatIncrement (Int1To10 maxdeg1, Int1To10 maxdeg2)  
        Int1To1000 ms = effortRepeatIncrement (Int1To1000 maxsize1, Int1To1000 maxsize2)  

