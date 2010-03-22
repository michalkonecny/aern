{-|
    Module      :  Numeric.AERN.Misc.Debug
    Description :  miscellaneous debugging functions  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Miscellaneous debugging functions.
-}

module Numeric.AERN.Misc.Debug where

import System.IO.Unsafe

unsafePrint msg val =
    unsafePerformIO $
        do
        putStrLn $ "unsafe: " ++ msg
        return val

unsafePrintReturn msg a =
    unsafePrint (msg ++ show a) a
