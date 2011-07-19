{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Domain
    Description :  operations focusing on function domains  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations focusing on function domains.
-}

module Numeric.AERN.RmToRn.Domain where

import Numeric.AERN.Basics.Interval

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap


class (HasVarValue 
        (VarBox f (Interval (Domain f))) 
        (Var f) 
        (Interval (Domain f))) 
        => HasDomainBox f 
    where
    type Var f
    type Domain f
    type VarBox f :: * -> *
    getSampleDomValue :: f -> Domain f
--    getFreshVariable :: f -> [Var f] -> Var f
    getNVariables :: f -> Int -> [Var f]
    getDomainBox :: f -> DomainBox f
    defaultDomSplit ::
        f {-^ dummy parameter that aids typechecking -} -> 
        (Interval (Domain f)) -> 
        (Interval (Domain f), Interval (Domain f))

type DomainBox f = VarBox f (Interval (Domain f))
    

--type DomainPointBox f = VarBox f (Domain f)
--
--class (HasDomainBox f, 
--       HasVarValue 
--        (VarBox f (Domain f)) 
--        (Var f) 
--        (Domain f)) => HasDomainPtBox f

class HasVarValue vbox var val 
    | vbox -> var val
    where
    unitVarBox :: var -> val -> vbox
    fromList :: [(var, val)] -> vbox
    fromAscList :: [(var, val)] -> vbox
    toAscList :: vbox -> [(var, val)]
    getVars :: vbox -> [var]
    lookupVar :: vbox -> var -> Maybe val
    -- TODO add much more (see hsreals DomainBox)


instance HasVarValue (IntMap.IntMap val) Int val
    where
    unitVarBox var val = IntMap.singleton var val
    fromList varVals = IntMap.fromList varVals
    fromAscList varVals = IntMap.fromAscList varVals
    toAscList vbox = IntMap.toAscList vbox
    getVars vbox = IntMap.keys vbox
    lookupVar map var = IntMap.lookup var map 

instance (Ord var) => HasVarValue (Map.Map var val) var val
    where
    unitVarBox var val = Map.singleton var val
    fromList varVals = Map.fromList varVals
    fromAscList varVals = Map.fromAscList varVals
    toAscList vbox = Map.toAscList vbox
    getVars vbox = Map.keys vbox
    lookupVar map var = Map.lookup var map 
     
           
