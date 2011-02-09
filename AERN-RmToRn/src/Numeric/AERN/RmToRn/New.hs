{-# LANGUAGE TypeFamilies #-}
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

class HasSizeLimits f where
    type SizeLimits f
    getSizeLimits :: f -> SizeLimits f

class (HasDomainBox f, HasSizeLimits f) => HasProjections f where
    newProjection :: 
        Maybe f {-^ dummy parameter that aids typechecking -} ->
        (SizeLimits f) {-^ limits of the new function -} -> 
        (DomainBox f) {-^ the domain @box@ of the function -} -> 
        (Var f) {-^ the variable @x@ being projected -} -> 
        f {-^ @ \box -> x @ -}
