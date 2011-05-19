{-# LANGUAGE TypeFamilies #-}
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

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.New where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain()

-- import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (PolyPure)
import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

import Numeric.AERN.Basics.Exception (AERNException(..))
import Control.Exception (throw)

import qualified Data.IntMap as Map
import qualified Data.List as List

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    => 
    HasSizeLimits (PolyPure cf)
    where
    type SizeLimits (PolyPure cf) = (Int, Int) -- maxSize, maxDegree
    getSizeLimits p = 
        (maxSize, maxDegree)
        where
        (_, Size maxSize32, Power maxDegree32) =
            runST $ 
                do
                pM <- unsafeMakeMutable p
                Poly.peekSizes pM
        maxSize = fromIntegral maxSize32
        maxDegree = fromIntegral maxDegree32
    defaultSizes p =
        (2 + 3 * arity, 3)
        where
        Var arity32 =
            runST $ 
                do
                pM <- unsafeMakeMutable p
                Poly.peekArity pM
        arity = fromIntegral arity32
        

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    => 
    HasProjections (PolyPure cf)
    where
    newProjection _ (maxSize, maxDegree) domainBox var =
        runST $
            do
            pM <- Poly.projectionPoly 
                    (Poly.Var $ fromIntegral var)
                    (Poly.Var $ fromIntegral arity)
                    (Poly.Size $ fromIntegral maxSize)
                    (Poly.Power $ fromIntegral maxDegree)
            unsafeReadMutable pM
        where
        arity 
            | domainBoxOK = length vars
            | otherwise = 
                throw $ AERNException "GenericCoeff polynomial newProjection called with illegal domain box"
        vars = Map.keys domainBox
        domainBoxOK =
            True
            &&
            (List.sort vars `List.isPrefixOf` [0,1..])

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    HasConstFns (PolyPure cf)
    where
    newConstFn _ (maxSize, maxDegree) domainBox value =
        runST $
            do
            pM <- Poly.constPoly 
                    value
                    zero -- radius. ie errorBound
                    (Poly.Var $ fromIntegral arity)
                    (Poly.Size $ fromIntegral maxSize)
                    (Poly.Power $ fromIntegral maxDegree)
            unsafeReadMutable pM
        where
        arity 
            | domainBoxOK = length vars
            | otherwise = 
                throw $ AERNException "GenericCoeff polynomial newProjection called with illegal domain box"
        vars = Map.keys domainBox
        domainBoxOK =
            True
            &&
            (List.sort vars `List.isPrefixOf` [0,1..])
            
