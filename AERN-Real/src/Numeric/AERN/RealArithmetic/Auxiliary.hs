{-|
    Module      :  Numeric.AERN.RealArithmetic.Auxiliary
    Description :  auxiliary generic operators and utilities  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Auxiliary generic operators and utilities.

    This is a hidden internal module.    
-}

module Numeric.AERN.RealArithmetic.Auxiliary where

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Mutable

import Control.Exception
import Control.Monad.ST


powerFromMult :: 
    (HasOne t) =>
    (t -> t -> t) {-^ associative binary operation @*@ -} ->
    t {-^ @x@ -} ->
    Int {-^ @n@ positive -} ->
    t {-^ product @x * x * ... * x@ of @n@ copies of @x@ -}
powerFromMult mult x n
    | n < 0 = throw $ AERNException "powerFromMult does not support negative exponents"
    | otherwise = p n
    where
    p n
        | n == 0 = one
        | n == 1 = x
        | otherwise =
            case even n of
                True -> 
                    powHalf `mult` powHalf 
                False -> 
                    x `mult` (powHalf `mult` powHalf)
        where
        powHalf = p (n `div` 2)

powerFromMultInPlace :: 
    (HasOne t, CanBeMutable t) =>
    (Mutable t s -> Mutable t s -> Mutable t s -> ST s ()) {-^ associative binary operation @*@ -} ->
    (Mutable t s) {-^ where to put the resulting power @x^n@  -} ->
    (Mutable t s) {-^ @x@ -} ->
    Int {-^ @n@ positive -} ->
    ST s ()
powerFromMultInPlace mult rM xM n
    -- beware rM and xM may alias!
    | n < 0 = throw $ AERNException "powerFromMultInPlace does not support negative exponents"
    | otherwise =
        do
        nrM <- cloneMutable xM -- a non-aliased variable for interim results
        p nrM n -- nrM := x^n
        assignMutable rM nrM -- rM := nr
    where
    p nrM n -- ensures nrM holds x^n
        | n == 0 = writeMutable nrM one
        | n == 1 = return () -- assuming nrM already contains x
        | otherwise =
            case even n of
                True -> 
                    do
                    powHalf -- rM now holds x^(n/2)
                    mult nrM nrM nrM -- square rM
                False -> 
                    do
                    powHalf -- rM now holds x^(n-1/2)
                    mult nrM nrM nrM -- square rM
                    mult nrM nrM xM -- multiply by x one more time
        where
        rM = () -- avoid accidental use of rM from parent context
        powHalf = p nrM (n `div` 2)

