---
title: Verifying a SPARK FP program, Part 1: Annotations (under construction)
author: Michal Konečný
date: 2014-02-28
mathjax: on
---

This tutorial shows the steps of writing a floating-point SPARK 2005 program
and proving it computes numbers that correspond to its intuitive meaning 
as well as proving the absence of numerical exceptions.  The complete code
for this program is available [here](https://github.com/michalkonecny/polypaver/tree/master/tools/SPARK2005/examples/erfRiemann).

The tutorial is divided into the following parts:

  1. **Annotations: Annotating a SPARK floating-point program** *(this page, under construction)* 
  2. Generation: Generating valid VCs *(coming soon)*
  3. Proving easier VCs: Proving most of the VCs and identifying challenging VCs *(coming soon)*
  4. Proving harder VCs: Attempting to prove the harder VCs *(coming soon)*
  5. Debugging: Detecting and locating mistakes in SPARK floating-point programs *(coming soon)* 

---

**TODO**: *Add TOC*



## The Ada program

The program computes an approximation of the integral

$$
\int_0^x e^{-t^2}\;\mathrm{d}t
$$

This integral is an essential part of the [Gauss error function](http://en.wikipedia.org/wiki/Error_function) and it has no closed algebraic solution.

The integral is approximated using the left Riemann sum over a partition with $2^n$ segments of equal size.  

The following is an Ada implementation of this method.

```ada
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body Riemann is

   function erf_Riemann(x : Float; n : Integer) return Float
   is
      partitionSize : Integer := 2 ** n;
      stepSize : Float := x/Float(partitionSize);
      tLeft : Float;
      valueLeft : Float;
      result : Float;
   begin

      result := 0.0;

      for step in 0..((2**n)-1) loop
         tLeft := stepSize * Float(step);
         valueLeft := Exp(-(tLeft * tLeft));
         result := result + stepSize * valueLeft;
      end loop;

      return result;
   end erf_Riemann;

end Riemann;
```

The above code is available in a ready-to-compile form in 
[this folder](https://github.com/michalkonecny/polypaver/tree/master/tools/SPARK2005/examples/erfRiemann/tutorial-steps/01-noSPARK).

To get an idea of how close the function is to the exact interval, there is a main procedure that prints
the results of the `erf_Riemann` function for a number of different `x` and `n`.  To execute this program,
issue the following commands:

```
> cd tools/SPARK2005/examples/erfRiemann/tutorial-steps/01-noSPARK
> mkdir -p build
> gnatmake -P erf_riemann.gpr
> build/main
```  

The output should start with the following:

```
erf_Riemann(1.0,  1) = 0.889400
erf_Riemann(1.0,  2) = 0.821999
erf_Riemann(1.0,  3) = 0.785373
erf_Riemann(1.0,  4) = 0.766338
erf_Riemann(1.0,  5) = 0.756641
erf_Riemann(1.0,  6) = 0.751748
erf_Riemann(1.0,  7) = 0.749290
erf_Riemann(1.0,  8) = 0.748058
erf_Riemann(1.0,  9) = 0.747441
erf_Riemann(1.0, 10) = 0.747132

the integral for x = 1.0: 0.746824
--------------------------------------------------------
```

With a partition of size 1024, the result agrees with the integral in the first two
significant digits.

## Precondition and postcondition

### Bounding the method error

Let us defer reasoning about rounding errors for later and first consider the
algorithm as if it was performed in exact real arithmetic and analyse its method error.

How does the Riemann sum relate to the integral?  We first restrict the domain of $x$ to non-negative numbers.
Thus the integrand $e^{-t^2}$ is considered only for non-negative $t$, where it is a decreasing function.
The left Riemann sum is therefore an *upper bound* on the exact integral.  

**TODO**: *Add diagram illustrating the sums*

Moreover, the difference between the left Riemann sum and the integral can be bounded by the difference
between the left and rigth Riemann sums, which happens to be exactly 

$$
\frac{x}{n}\left(1-e^{-x^2}\right)
$$

Base on this observation, one can construct the following post-condition for erf_Riemann:

$$  
\mathtt{result} 
\in  
\int_0^x e^{-t^2}\;\mathrm{d}t
+
\left[0, \frac{x}{n}\left(1-e^{-x^2}\right)\right]
$$

### Adapting for SPARK 

Let us now enter the above postcondition into the Ada specification
as a SPARK 2005 annotation:

```Ada
--# inherit PP_F_Exact;
package Riemann is 

   function erf_Riemann(x : Float; n : Integer) return Float;
   --# return result => 
   --#     PP_F_Exact.Contained_In(
   --#         result
   --#         ,
   --#         PP_F_Exact.Integral(0.0,x,PP_F_Exact.Exp(-PP_F_Exact.Integration_Variable**2))
   --#         +
   --#         PP_F_Exact.Interval(
   --#              0.0
   --#              ,
   --#              (1.0-PP_F_Exact.Exp(-x**2))*x/Float(2**n)
   --#         )
   --#     );

end Riemann;
```

**TODO**: *Explain PP_F_Exact*

**TODO**: *Show also the refactoring of the function body necessary to satisfy the Examiner*

<!--
The package `PP_F_Elementary` is a PolyPaver-friendly alternative
to `Ada.Numeric.Elementary_Functions`.  
This package is included in the PolyPaver download bundle.  
-->


### Bounding the rounding error

### Bounding the domain

## The loop invariant

*incremental version of the postcondition*

*bounding local variables*


