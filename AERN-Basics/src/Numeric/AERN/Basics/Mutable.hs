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
      A mutable version of the type t. 
      
      The extra parameter is the state of the ST monad run. -}
    data Mutable t :: * -> *

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
    assignMutable :: t -> Mutable t s -> Mutable t s -> ST s ()
    assignMutable sample rM aM =
        do
        a <- unsafeReadMutable aM
        let _ = [a, sample]
        writeMutable rM a 
    {-| Swap the values of two mutable variables  -}
    swapMutable :: t -> Mutable t s -> Mutable t s -> ST s ()
    swapMutable sample aM bM =
        do
        a <- unsafeReadMutable aM
        b <- unsafeReadMutable bM
        let _ = [a, b, sample]
        writeMutable aM b 
        writeMutable bM a 
    {-| Clone a mutable variable, the first parameter only aids type checking  -}
    cloneMutable :: t -> Mutable t s -> ST s (Mutable t s)
    cloneMutable sample aM =
        do
        a <- unsafeReadMutable aM
        let _ = [a, sample]
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
    t ->
    (t -> t) ->
    OpMutable1 t s
pureToMutable1 sample pureFn resM aM =
    do
    a <- readMutable aM
    let _ = [a,sample]
    unsafeWriteMutable resM (pureFn a)

pureToMutable1Eff ::
    (CanBeMutable t) =>
    t ->
    (eff -> t -> t) ->
    OpMutable1Eff eff t s
pureToMutable1Eff sample pureFn eff resM aM =
    do
    a <- readMutable aM
    let _ = [a,sample]
    unsafeWriteMutable resM (pureFn eff a)

pureToMutable2Eff ::
    (CanBeMutable t) =>
    t ->
    (eff -> t -> t -> t) ->
    OpMutable2Eff eff t s
pureToMutable2Eff sample pureFn eff resM aM bM =
    do
    a <- readMutable aM
    let _ = [a,sample]
    b <- readMutable bM
    unsafeWriteMutable resM (pureFn eff a b)

pureToMutableNonmutEff ::
    (CanBeMutable t) =>
    t ->
    (eff -> t -> nonmut -> t) ->
    OpMutableNonmutEff eff t nonmut s
pureToMutableNonmutEff sample pureFn eff resM aM b =
    do
    a <- readMutable aM
    let _ = [a,sample]
    unsafeWriteMutable resM (pureFn eff a b)

--propWriteRead :: 
--propWriteWriteRead ::
--propWriteWriteReadConcurrent ::
