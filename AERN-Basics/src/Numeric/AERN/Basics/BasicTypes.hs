{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.BasicTypes
    Description :  auxiliary types for exact real number processing 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    auxiliary types for exact real number processing
-}
module Numeric.AERN.Basics.BasicTypes where

{-|
  The bit size of the floating point numbers (or similar)
  used internally in real number and function approximations.
-}
type Granularity = Int

