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
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (Poly)
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
    HasSizeLimits (Poly cf)
    where
    type SizeLimits (Poly cf) = 
        (Poly.OpsFP cf, Int, Int) -- maxSize, maxDegree
    getSizeLimits p@(Poly.Poly opsFP _) = 
        (opsFP, maxSize, maxDegree)
        where
        (_, Size maxSize32, Power maxDegree32) = Poly.peekSizes p
        maxSize = fromIntegral maxSize32
        maxDegree = fromIntegral maxDegree32
    defaultSizes p =
        (opsFP, 2 + 3 * arity, 3)
        where
        Var arity32 = Poly.peekArity p
        arity = fromIntegral arity32
        opsFP = Poly.opsFPArithUpDnDefaultEffort (Poly.peekConst p)
        

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    => 
    HasProjections (Poly cf)
    where
    newProjection (opsFP, maxSize, maxDegree) domainBox var =
        Poly.projectionPoly
            opsFP 
            (Poly.Var $ fromIntegral arity)
            (Poly.Size $ fromIntegral maxSize)
            (Poly.Power $ fromIntegral maxDegree)
            (Poly.Var $ fromIntegral var)
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
    HasConstFns (Poly cf)
    where
    newConstFn (opsFP, maxSize, maxDegree) domainBox value =
        Poly.constPoly
            opsFP 
            (Poly.Var $ fromIntegral arity)
            (Poly.Size $ fromIntegral maxSize)
            (Poly.Power $ fromIntegral maxDegree)
            value
            zero -- radius. ie errorBound
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
            
