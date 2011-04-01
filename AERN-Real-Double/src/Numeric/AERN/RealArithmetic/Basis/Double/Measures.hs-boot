{-
  this file is needed to break the dependency cycle:

  Numeric.AERN.RealArithmetic.Basis.Double
     imports: Numeric.AERN.RealArithmetic.Basis.Double.Measures
  Numeric.AERN.RealArithmetic.Basis.Double.Measures
    imports: Numeric.AERN.RealArithmetic.Interval.Double
  Numeric.AERN.RealArithmetic.Interval.Double
    imports: Numeric.AERN.RealArithmetic.Basis.Double
-}

module Numeric.AERN.RealArithmetic.Basis.Double.Measures where