{-# LANGUAGE FlexibleContexts #-}

module Numeric.AERN.RmToRn.RefinementOrderRounding.Substitute where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Exception

import Control.Exception

class (HasDomainBox f, HasVarValue (VarBox f f) (Var f) f) => 
        CanSubstitute f where
    substitute :: 
        f {-^ function @f@ -} ->
        (VarBox f f)
            {-^ functions @f_x@ to substitute for some variables @x@ -} ->
        (f,f)
            {-^ outwards and inwards rounded 
                @\x... -> f(x -> f_x,...)@ -}
           
splitDomain ::
    (CanSubstitute f) =>
    f -> 
    (Var f) 
        {-^ the variable whose domain to split -} ->
    (Maybe (Domain f))
        {-^ optionally, at which point to split the domain -} ->
    ((f,f),(f,f))
        {-^ both results, each of them downwards and upwards rounded -}

splitDomain f var maybePt =
    (substitute f leftVarDomDB,
     substitute f rightVarDomDB)
    where
    leftVarDomDB = unitVarBox var leftDomTransl
    rightVarDomDB = unitVarBox var rightDomTransl
    leftDomTransl = error "AERN-RmToRn: splitDomain not implemented yet"
    rightDomTransl = error "AERN-RmToRn: splitDomain not implemented yet"
    (leftDom, rightDom) =
        case maybePt of
            Nothing -> defaultDomSplit (Just f) varDom
            Just pt -> (Interval domLo pt, Interval pt domHi)
    varDom@(Interval domLo domHi) = 
        case lookupVar fDomainBox  var of
            Just d -> d
            Nothing -> throw $ AERNException $ "splitDomain applied on non-existent variable"
    fDomainBox = getDomainBox f
