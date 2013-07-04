{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.New
    Description :  constructors of basic functions  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Constructors of basic functions.
-}

module Numeric.AERN.RmToRn.New where

import Numeric.AERN.RmToRn.Domain

import Numeric.AERN.Basics.Effort

class HasSizeLimits f where
    type SizeLimits f
    getSizeLimits :: f -> SizeLimits f
    defaultSizeLimits :: f -> SizeLimits f
    adjustSizeLimitsToVarsAndDombox :: f -> [Var f] -> DomainBox f -> SizeLimits f -> SizeLimits f

class
    (EffortIndicator (SizeLimitsChangeEffort f)) 
    => 
    CanChangeSizeLimits f 
    where
    type SizeLimitsChangeEffort f
    sizeLimitsChangeDefaultEffort :: f -> SizeLimitsChangeEffort f
    changeSizeLimitsOutEff :: SizeLimitsChangeEffort f -> SizeLimits f -> f -> f
    changeSizeLimitsInEff :: SizeLimitsChangeEffort f -> SizeLimits f -> f -> f

changeSizeLimitsOut :: 
    CanChangeSizeLimits f => 
    SizeLimits f -> f -> f
changeSizeLimitsOut limits a =
    changeSizeLimitsOutEff (sizeLimitsChangeDefaultEffort a) limits a 

changeSizeLimitsIn :: 
    CanChangeSizeLimits f => 
    SizeLimits f -> f -> f
changeSizeLimitsIn limits a =
    changeSizeLimitsInEff (sizeLimitsChangeDefaultEffort a) limits a 

class (HasDomainBox f, HasSizeLimits f) => HasProjections f where
    newProjection :: 
        (SizeLimits f) {-^ limits of the new function -} -> 
        (DomainBox f) {-^ the domain @box@ of the function -} -> 
        (Var f) {-^ the variable @x@ being projected -} -> 
        f {-^ @ \box -> x @ -}

newProjectionFromSample ::
    (HasProjections f) =>
    f -> (Var f) -> f
newProjectionFromSample sampleF var =
    newProjection sizeLimits domBox var
    where
    sizeLimits = getSizeLimits sampleF
    domBox = getDomainBox sampleF

class (HasDomainBox f, HasSizeLimits f) => HasConstFns f where
    newConstFn :: 
        (SizeLimits f) {-^ limits of the new function -} -> 
        (DomainBox f) {-^ the domain @box@ of the function -} -> 
        (Domain f) {-^ the value @v@ of the constant function -} -> 
        f {-^ @ \box -> v @ -}

class (HasDomainBox f) => CanAddVariables f where
    addVariablesFront ::
        [(Var f, Domain f)] ->
        f ->
        f
    addVariablesBack ::
        [(Var f, Domain f)] ->
        f ->
        f

class (HasDomainBox f) => CanRenameVariables f where
    renameVar :: (Var f) -> (Var f) -> f -> f
    renameVars :: (Var f -> Var f) -> f -> f
    
newConstFnFromSample ::
    (HasConstFns f) =>
    f -> (Domain f) -> f
newConstFnFromSample sampleF value =
    newConstFn sizeLimits domBox value
    where
    sizeLimits = getSizeLimits sampleF
    domBox = getDomainBox sampleF
    
