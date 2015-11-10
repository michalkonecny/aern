module FnReps.FunctionAbstraction where

import Numeric.AERN.MPFRBasis.Interval

class (Floating fn) => RF fn where
    evalMI :: fn -> MI -> MI
    constFn :: MI -> fn
    idFn :: fn
    weierstrassFn :: fn
    primitiveFn :: fn -> fn
    


