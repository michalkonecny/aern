{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain
    Description :  implementation of operations focusing on function domains
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations focusing on function domains.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain where

import Numeric.AERN.RmToRn.Domain
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly (PolyFP)

import Numeric.AERN.Basics.Interval (Interval(..))

import Numeric.AERN.Basics.Exception (AERNException(..))
import Control.Exception (throw)

import qualified Data.IntMap as Map

instance HasDomainBox PolyFP
    where
    type Var PolyFP = Int
    type Domain PolyFP = Double
    type VarBox PolyFP = Map.IntMap
       {- the set of variables is always 0..(n-1) -}
    getDomainBox p = 
        case Poly.peekArity p of 
            Poly.Var n32 ->
                Map.fromAscList $ map (\n -> (n,Interval (-1) 1)) [0..(n-1)]
                where
                n = fromInteger $ toInteger n32  
    defaultDomSplit _ _ = 
        throw $ AERNException "cannot split the domain of a PolyFP"
        
