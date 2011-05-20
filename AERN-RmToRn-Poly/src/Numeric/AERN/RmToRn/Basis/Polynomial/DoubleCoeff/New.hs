{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.New
    Description :  implementation of function constructors
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Implementation of function constructors.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.New where

import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain()

-- import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly (PolyFP)
import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics

import Numeric.AERN.Basics.Exception (AERNException(..))
import Control.Exception (throw)

import qualified Data.IntMap as Map
import qualified Data.List as List

instance HasSizeLimits PolyFP
    where
    type SizeLimits PolyFP = (Int, Int) -- maxSize, maxDegree
    getSizeLimits p = 
        (maxSize, maxDegree)
        where
        (_, Size maxSize32, Power maxDegree32) = Poly.peekSizes p
        maxSize = fromIntegral maxSize32
        maxDegree = fromIntegral maxDegree32

instance HasProjections PolyFP
    where
    newProjection (maxSize, maxDegree) domainBox var =
        Poly.projectionPoly 
            (Poly.Var $ fromIntegral var)
            (Poly.Var $ fromIntegral arity)
            (Poly.Size $ fromIntegral maxSize)
            (Poly.Power $ fromIntegral maxDegree)
        where
        arity 
            | domainBoxOK = length vars
            | otherwise = 
                throw $ AERNException "DoubleCoeff polynomial newProjection called with illegal domain box"
        vars = Map.keys domainBox
        domainBoxOK =
            True
            &&
            (List.sort vars `List.isPrefixOf` [0,1..])

instance HasConstFns PolyFP
    where
    newConstFn (maxSize, maxDegree) domainBox value =
        Poly.constPoly 
            value
            0 -- radius. ie errorBound
            (Poly.Var $ fromIntegral arity)
            (Poly.Size $ fromIntegral maxSize)
            (Poly.Power $ fromIntegral maxDegree)
        where
        arity 
            | domainBoxOK = length vars
            | otherwise = 
                throw $ AERNException "DoubleCoeff polynomial newProjection called with illegal domain box"
        vars = Map.keys domainBox
        domainBoxOK =
            True
            &&
            (List.sort vars `List.isPrefixOf` [0,1..])
            
