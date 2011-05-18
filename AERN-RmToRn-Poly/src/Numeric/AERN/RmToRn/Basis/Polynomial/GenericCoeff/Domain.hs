{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain
    Description :  implementation of operations focusing on function domains
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations focusing on function domains.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain where

import Numeric.AERN.RmToRn.Domain
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (PolyPure)

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Interval (Interval(..))

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

import Numeric.AERN.Basics.Exception (AERNException(..))
import Control.Exception (throw)


import qualified Data.IntMap as Map

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    => 
    HasDomainBox (PolyPure cf)
    where
    type Var (PolyPure cf) = Int
    type Domain (PolyPure cf) = cf
    type VarBox (PolyPure cf) = Map.IntMap
       {- the set of variables is always 0..(n-1) -}
    getDomainBox p =
        Map.fromAscList $ map (\n -> (n,domain)) [0..(n-1)]
        where
        domain = Interval (neg one) one
        n = fromInteger $ toInteger n32  
        (Poly.Var n32) = 
            runST $
                do
                pM <- unsafeMakeMutable p
                Poly.peekArity pM
    defaultDomSplit _ _ = 
        throw $ AERNException "cannot split the domain of a PolyPure"
        
