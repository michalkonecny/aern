{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.New
    Description :  implementation of function constructors
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Implementation of function constructors.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.New 
(
    PolySizeLimits(..)
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (Poly)
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff 
    (OpsFP,opsFPArithUpDnDefaultEffort)

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.NumericOrder as NumOrd

import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

import Numeric.AERN.Basics.Exception (AERNException(..))
import Control.Exception (throw)

import qualified Data.IntMap as IntMap
import qualified Data.List as List

import Test.QuickCheck

instance 
    (ArithUpDn.RoundedRealInPlace cf, HasLegalValues cf, Storable cf, Show cf) 
    => 
    HasSizeLimits (Poly cf)
    where
    type SizeLimits (Poly cf) = PolySizeLimits cf (Poly cf) 
    getSizeLimits p@(Poly.Poly opsFP _) = 
        PolySizeLimits p c opsFP  maxSize maxDegree maxTermArity
        where
        c = Poly.peekConst p
        (_, Poly.Size maxSize32, 
         Poly.Power maxDegree32, Poly.Var maxTermArity32) = Poly.peekSizes p
        maxSize = fromIntegral maxSize32
        maxDegree = fromIntegral maxDegree32
        maxTermArity = fromIntegral maxTermArity32
    defaultSizes p =
        PolySizeLimits p c opsFP (2 + 3 * arity) 3 (min 10 arity)
        where
        c = Poly.peekConst p
        Poly.Var arity32 = Poly.peekArity p
        arity = fromIntegral arity32
        opsFP = opsFPArithUpDnDefaultEffort (Poly.peekConst p)

data PolySizeLimits cf p =
    PolySizeLimits
    {
        polyLimsSampleP :: p,
        polyLimsSampleCoeff :: cf, 
        polyLimsOps :: OpsFP cf,
        polyLimsMaxSize :: Int,
        polyLimsMaxDegree :: Int,
        polyLimsMaxTermArity :: Int
    }

instance (HasDomainBox p) => Show (PolySizeLimits cf p)
    where
    show (PolySizeLimits p _ _ maxSize maxDeg maxTermArity) =
        "{maxArity = " ++ show maxArity 
        ++ ",maxSize=" ++ show maxSize 
        ++ ",maxDeg=" ++ show maxDeg 
        ++ ",maxTermArity=" 
        ++ show maxTermArity ++ "}"
        where
        maxArity = length $ toAscList $ getDomainBox p

instance 
    (ArithUpDn.RoundedRealInPlace cf, HasLegalValues cf, Storable cf, Show cf) 
    => 
    HasProjections (Poly cf)
    where
    newProjection (PolySizeLimits _ _ opsFP maxSize maxDegree maxTermArity) domainBox var =
        Poly.projectionPoly
            opsFP 
            (Poly.Var $ fromIntegral arity)
            (Poly.Size $ fromIntegral maxSize)
            (Poly.Power $ fromIntegral maxDegree)
            (Poly.Var $ fromIntegral maxTermArity)
            (Poly.Var $ fromIntegral var)
        where
        arity 
            | domainBoxOK = length vars
            | otherwise = 
                throw $ AERNException "GenericCoeff polynomial newProjection called with illegal domain box"
        vars = IntMap.keys domainBox
        domainBoxOK =
            True
            &&
            (List.sort vars `List.isPrefixOf` [0,1..])

instance 
    (ArithUpDn.RoundedRealInPlace cf, HasLegalValues cf, Storable cf, Show cf) 
    =>
    HasConstFns (Poly cf)
    where
    newConstFn (PolySizeLimits _ _ opsFP maxSize maxDegree maxTermArity) domainBox value =
        Poly.constPoly
            opsFP 
            (Poly.Var $ fromIntegral arity)
            (Poly.Size $ fromIntegral maxSize)
            (Poly.Power $ fromIntegral maxDegree)
            (Poly.Var $ fromIntegral maxTermArity)
            value
            zero -- radius. ie errorBound
        where
        arity 
            | domainBoxOK = length vars
            | otherwise = 
                throw $ AERNException "GenericCoeff polynomial newProjection called with illegal domain box"
        vars = IntMap.keys domainBox
        domainBoxOK =
            True
            &&
            (List.sort vars `List.isPrefixOf` [0,1..])
            
instance
    (ArithUpDn.RoundedRealInPlace cf, HasLegalValues cf, Storable cf, Show cf) 
    =>
    HasZero (Poly cf)
    where
    zero = newConstFnZeroSize zero
    
instance
    (ArithUpDn.RoundedRealInPlace cf, HasLegalValues cf, Storable cf, Show cf) 
    =>
    HasOne (Poly cf)
    where
    one = newConstFnZeroSize one
    
instance
    (ArithUpDn.RoundedRealInPlace cf,
     HasLegalValues cf,
     NumOrd.HasGreatest cf, 
     Storable cf, Show cf) 
    =>
    NumOrd.HasGreatest (Poly cf)
    where
    greatest = newConstFnZeroSize NumOrd.greatest
    
instance
    (ArithUpDn.RoundedRealInPlace cf, 
     HasLegalValues cf,
     NumOrd.HasLeast cf, 
     Storable cf, Show cf) 
    =>
    NumOrd.HasLeast (Poly cf)
    where
    least = newConstFnZeroSize NumOrd.least

instance
    (ArithUpDn.RoundedRealInPlace cf,
     HasLegalValues cf, 
     NumOrd.HasExtrema cf, 
     Storable cf, Show cf) 
    =>
    NumOrd.HasExtrema (Poly cf)
    
newConstFnZeroSize c = p
    where
    p =
        Poly.constPoly
            opsFP 
            (Poly.Var 0)
            (Poly.Size 0)
            (Poly.Power 0)
            (Poly.Var 0)
            c
            zero -- radius. ie errorBound
    opsFP = polyLimsOps $ defaultSizes p
            
instance
    (ArithUpDn.RoundedRealInPlace cf, HasLegalValues cf, Storable cf, Show cf) 
    =>
    Arbitrary (PolySizeLimits cf (Poly cf))
    where
    arbitrary =
        sized $ \size ->
        do
        let maxArity = 2 + (size `div` 7)
        maxDegree <- choose (1,2 + (size `div` 7))
        maxSize <- choose (3,10 + 3* size)
        maxTermArity <- choose (2, min 12 maxArity)
        let opsFP = polyLimsOps $ defaultSizes dummyP
            dummyP = newConstFn sizes box dummyCoeff
            dummyCoeff = zero
            box = domainBox maxArity
            sizes = PolySizeLimits dummyP dummyCoeff opsFP maxSize maxDegree maxTermArity
        return sizes 
        where
        domainBox arity
            = fromAscList $ zip [0,1..arity-1] $ repeat unitInterval
            where
            unitInterval = Interval (neg one) one

        
        

 