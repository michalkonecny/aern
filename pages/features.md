---
title: PolyPaver | Features
author: Michal Konečný
date: 2014-02-24
mathjax: on
---

### In version 0.3:

* Deciding conjectures with expressions that contain 
    * The operations +,-,*,/, abs, sqrt, exp <!--, sin, cos -->
    * Explicit **intervals** and **interval operations**
    * Definite **integrals** (with arbitrary expressions for endpoints)
    * Floating-point **rounded operations** (with adjustable precision)
* Verification of **floating-point** [SPARK/Ada 2005](http://en.wikipedia.org/wiki/SPARK_(programming_language)) code
    * Full verification of floating-point code in cooperation with the official SPARK tools
         * Functional correctness
         * Freedom from numerical exceptions
    * Annotations can specify 
         * a bound on rounding errors, as a function of input values
         * a relation of floating-point results with intuitive mathematical meaning  
    * Specifications can contain interval operations and integrals
    * Effective for procedures and functions with up to 4-6 floating-point variables
       *(arrays are not supported yet)*
* Live graphical progress update for 2D problems

### Planned:

  * Support for other elementary real functions
  * Integration with [SPARK 2014](http://www.spark-2014.org/)
  * Support for formulas in TPTP format as used in [MetiTarski](http://www.cl.cam.ac.uk/~lp15/papers/Arith/)
  * Integration with symbolic theorem provers, SMT
  * Proofs checkable by a widely trusted theorem prover




