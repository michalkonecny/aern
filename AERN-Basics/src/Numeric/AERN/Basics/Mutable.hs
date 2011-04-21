{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  Numeric.AERN.Basics.Mutable
    Description :  a type class for ST mutable structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    A type class for ST mutable structures.
-}
module Numeric.AERN.Basics.Mutable where

import Control.Monad.ST (ST, runST)

class CanBeMutable t where
    {-| 
      A mutable version of the type t. The extra parameter is the state of the ST monad run. -}
    data Mutable t :: * -> *

    {-| 
      Helper method for extracting the underlying type of a mutable value. Intended only for type inference, 
      throws error when evaluated. -}
    getDummySample :: Mutable t s -> t
    getDummySample _ =
        error "AERN internal error: getDummySample should never be evaluated, it serves only type inference"

    {-| Safely create a new mutable variable with the given value -}
    makeMutable :: t -> ST s (Mutable t s)
    {-| Create a new mutable variable with the given value, making the value volatile -}
    unsafeMakeMutable :: t -> ST s (Mutable t s)
    {-| A safe write/update operation -}
    writeMutable :: Mutable t s -> t -> ST s ()
    {-| An unsafe write/update operation; it makes the second argument volatile -}
    unsafeWriteMutable :: Mutable t s -> t -> ST s ()
    {-| A safe read operation, yielding an immutable value -}
    readMutable :: Mutable t s -> ST s t
    {-| An unsafe read operation, yielding an immutable value that may be volatile -}
    unsafeReadMutable :: Mutable t s -> ST s t
    {-| Assign a value from one mutable variable to another  -}
    assignMutable :: Mutable t s -> Mutable t s -> ST s ()
    assignMutable rM aM =
        do
        a <- unsafeReadMutable aM
        writeMutable rM a 
    {-| Swap the values of two mutable variables  -}
    swapMutable :: Mutable t s -> Mutable t s -> ST s ()
    swapMutable aM bM =
        do
        a <- unsafeReadMutable aM
        b <- unsafeReadMutable bM
        writeMutable aM b 
        writeMutable bM a 
    {-| Clone a mutable variable, the first parameter only aids type checking  -}
    cloneMutable :: Mutable t s -> ST s (Mutable t s)
    cloneMutable aM =
        do
        a <- unsafeReadMutable aM
        makeMutable a 

type OpMutable1 t s = 
    (Mutable t s) -> (Mutable t s) -> ST s () 

type OpMutable1Eff ei t s = 
    ei -> (Mutable t s) -> (Mutable t s) -> ST s () 

type OpMutable2 t s = 
    (Mutable t s) -> (Mutable t s) -> (Mutable t s) -> ST s () 

type OpMutable2Eff ei t s = 
    ei -> (Mutable t s) -> (Mutable t s) -> (Mutable t s) -> ST s () 

type OpMutableNonmut t nonmut s = 
    (Mutable t s) -> (Mutable t s) -> nonmut -> ST s () 

type OpNonmut t nonmut s = 
    (Mutable t s) -> nonmut -> ST s () 

type OpMutableNonmutEff ei t nonmut s = 
    ei -> (Mutable t s) -> (Mutable t s) -> nonmut -> ST s () 

mutable1ToPure ::
    (CanBeMutable t) =>
    (forall s. OpMutable1 t s) ->
    (t -> t)
mutable1ToPure mutableFn a =
    runST $
        do
        aM <- makeMutable a
        mutableFn aM aM
        unsafeReadMutable aM

mutable1EffToPure ::
    (CanBeMutable t) =>
    (forall s. OpMutable1Eff eff t s) ->
    (eff -> t -> t)
mutable1EffToPure mutableFn eff a =
    runST $
        do
        aM <- makeMutable a
        mutableFn eff aM aM
        unsafeReadMutable aM

mutable2ToPure ::
    (CanBeMutable t) =>
    (forall s. OpMutable2 t s) ->
    (t -> t -> t)
mutable2ToPure mutableFn a b =
    runST $
        do
        aM <- makeMutable a
        bM <- makeMutable b
        mutableFn aM aM bM
        unsafeReadMutable aM

mutable2EffToPure ::
    (CanBeMutable t) =>
    (forall s. OpMutable2Eff eff t s) ->
    (eff -> t -> t -> t)
mutable2EffToPure mutableFn eff a b =
    runST $
        do
        aM <- makeMutable a
        bM <- unsafeMakeMutable b
        mutableFn eff aM aM bM
        unsafeReadMutable aM

mutableNonmutEffToPure ::
    (CanBeMutable t) =>
    (forall s. OpMutableNonmutEff eff t nonmut s) ->
    (eff -> t -> nonmut -> t)
mutableNonmutEffToPure mutableFn eff a b =
    runST $
        do
        aM <- makeMutable a
        mutableFn eff aM aM b
        unsafeReadMutable aM

pureToMutable1 ::
    (CanBeMutable t) =>
    (t -> t) ->
    OpMutable1 t s
pureToMutable1 pureFn resM aM =
    do
    a <- readMutable aM
    unsafeWriteMutable resM (pureFn a)

pureToMutable2 ::
    (CanBeMutable t) =>
    (t -> t -> t) ->
    OpMutable2 t s
pureToMutable2 pureFn resM aM bM =
    do
    a <- readMutable aM
    b <- readMutable bM
    unsafeWriteMutable resM (pureFn a b)

pureToMutable1Eff ::
    (CanBeMutable t) =>
    (eff -> t -> t) ->
    OpMutable1Eff eff t s
pureToMutable1Eff pureFn eff resM aM =
    do
    a <- readMutable aM
    unsafeWriteMutable resM (pureFn eff a)

pureToMutable2Eff ::
    (CanBeMutable t) =>
    (eff -> t -> t -> t) ->
    OpMutable2Eff eff t s
pureToMutable2Eff pureFn eff resM aM bM =
    do
    a <- readMutable aM
    b <- readMutable bM
    unsafeWriteMutable resM (pureFn eff a b)

pureToMutableNonmutEff ::
    (CanBeMutable t) =>
    (eff -> t -> nonmut -> t) ->
    OpMutableNonmutEff eff t nonmut s
pureToMutableNonmutEff pureFn eff resM aM b =
    do
    a <- readMutable aM
    unsafeWriteMutable resM (pureFn eff a b)

pureEffToMutable1 ::
    (CanBeMutable t) =>
    (eff -> t -> t) ->
    (t -> eff) ->
    OpMutable1 t s
pureEffToMutable1 pureEffFn defEff resM aM =
    do
    d <- readMutable resM
    res <- readMutable resM
    a <- readMutable aM
    unsafeWriteMutable resM (pureEffFn (defEff d) a)

pureEffToMutable2 ::
    (CanBeMutable t) =>
    (eff -> t -> t -> t) ->
    (t -> eff) ->
    OpMutable2 t s
pureEffToMutable2 pureEffFn defEff resM aM bM =
    do
    d <- readMutable resM
    res <- readMutable resM
    a <- readMutable aM
    b <- readMutable bM
    unsafeWriteMutable resM (pureEffFn (defEff d) a b)

pureEffToMutableNonmut ::
    (CanBeMutable t) =>
    (eff -> t -> nonmut -> t) ->
    (t -> eff) ->
    OpMutableNonmut t nonmut s
pureEffToMutableNonmut pureEffFn defEff resM aM b =
    do
    d <- readMutable resM
    a <- readMutable aM
    unsafeWriteMutable resM (pureEffFn (defEff d) a b)

pureMixedEffToMutableNonmut ::
    (CanBeMutable t) =>
    (eff -> t -> nonmut -> t) ->
    (t -> nonmut -> eff) ->
    OpMutableNonmut t nonmut s
pureMixedEffToMutableNonmut pureEffFn defEff resM aM b =
    do
    d <- readMutable resM
    a <- readMutable aM
    unsafeWriteMutable resM (pureEffFn (defEff d b) a b)

--propWriteRead :: 
--propWriteWriteRead ::
--propWriteWriteReadConcurrent ::
