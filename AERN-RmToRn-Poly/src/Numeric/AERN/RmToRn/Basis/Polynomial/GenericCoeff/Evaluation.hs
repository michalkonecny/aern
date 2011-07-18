{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Evaluation
    Description :  evaluation of a PolyFP in a custom interpretation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation of a PolyFP in a custom interpretation.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Evaluation where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (Poly)
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

data PolyEvalOps cf t =
    PolyEvalOps
    {
        polyEvalFromCoeff :: cf -> t,
        polyEvalTimes :: t -> t -> t,
        polyEvalPlus :: t -> t -> t,
        polyEvalMinus :: t -> t -> t
    }

instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    => 
    CanEvaluateOtherType (Poly cf) 
    where
    type EvalOps (Poly cf) = PolyEvalOps cf
    evalOtherType evalOps varValuesBox p =
        evalAtPtChebBasis p
            (map snd $ toAscList varValuesBox)
            (polyEvalFromCoeff evalOps one)
            (polyEvalPlus evalOps)
            (polyEvalMinus evalOps)
            (polyEvalTimes evalOps)
            (polyEvalFromCoeff evalOps)

