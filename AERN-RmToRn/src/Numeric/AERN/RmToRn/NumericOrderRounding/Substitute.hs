
module Numeric.AERN.RmToRn.NumericOrderRounding.Substitute where

import Numeric.AERN.RmToRn.Domain

class (HasDomainBox f) => 
        CanSubstitute f where
    substitute :: 
        f {-^ function @f@ -} ->
        (VarBox f f)
            {-^ functions @f_x@ to substitute for some variables @x@ -} ->
        (f,f)
            {-^ outwards and inwards rounded 
                @\x... -> f(x -> f_x,...)@ -}
           
