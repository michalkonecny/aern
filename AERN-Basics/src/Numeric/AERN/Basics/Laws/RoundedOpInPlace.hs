{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  Numeric.AERN.Basics.Laws.InPlace
    Description :  common properties of in-place operations 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common properties of in-place operations, mainly consistency with 
    pure operations.
-}

module Numeric.AERN.Basics.Laws.RoundedOpInPlace where

import Numeric.AERN.Basics.Laws.Utilities
import Numeric.AERN.Basics.Mutable

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool

import Data.Maybe

inPlaceConsistentWithPure2 ::
    (CanBeMutable t) =>
    (PRel t) ->
    (forall s.OpMutable2 t s) ->
    (forall s.OpMutable2 t s) ->
    (Op t) ->
    (Op t) ->
    t ->
    t ->
    Bool
inPlaceConsistentWithPure2 (<=?)
        opDnInPlace opUpInPlace
        opDnPure opUpPure
        e1 e2 =
    (defined (e1 `opDnInPlacePurified` e2 <=? (e1 `opUpPure` e2)) 
        ===> (e1 `opDnInPlacePurified` e2 <= (e1 `opUpPure` e2))) 
    && 
    (defined (e1 `opDnPure` e2 <=? (e1 `opUpInPlacePurified` e2)) 
        ===> (e1 `opDnPure` e2 <= (e1 `opUpInPlacePurified` e2))) 
    where
    (<=) = assumeTotal2 (<=?)
    opDnInPlacePurified = mutable2ToPure opDnInPlace
    opUpInPlacePurified = mutable2ToPure opUpInPlace

inPlaceConsistentWithPurePartial2 ::
    (CanBeMutable t) =>
    (PRel t) ->
    (forall s.OpPartialMutable2 t s) ->
    (forall s.OpPartialMutable2 t s) ->
    (PartOp t) ->
    (PartOp t) ->
    t ->
    t ->
    Bool
inPlaceConsistentWithPurePartial2 (<=?)
        opDnInPlace opUpInPlace
        opDnPure opUpPure
        e1 e2 =
    (and $ map isJust $
        [e1OpDnInPlacePurifiedE2,  e1OpUpPureE2, e1OpDnInPlacePurifiedE2, e1OpUpPureE2])
    ===>
    ((defined (fromJust e1OpDnInPlacePurifiedE2 <=? (fromJust e1OpUpPureE2)) 
        ===> (fromJust e1OpDnInPlacePurifiedE2 <= (fromJust e1OpUpPureE2))) 
    && 
    (defined (fromJust e1OpDnPureE2 <=? (fromJust e1OpUpInPlacePurifiedE2)) 
        ===> (fromJust e1OpDnPureE2 <= (fromJust e1OpUpInPlacePurifiedE2)))) 
    where
    e1OpDnInPlacePurifiedE2 = e1 `opDnInPlacePurified` e2
    e1OpUpInPlacePurifiedE2 = e1 `opUpInPlacePurified` e2
    e1OpDnPureE2 = e1 `opDnPure` e2
    e1OpUpPureE2 = e1 `opUpPure` e2
    (<=) = assumeTotal2 (<=?)
    opDnInPlacePurified = mutablePartial2ToPure opDnInPlace
    opUpInPlacePurified = mutablePartial2ToPure opUpInPlace
    