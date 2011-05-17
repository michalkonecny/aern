#ifndef POLY_H_
#define POLY_H_

#include <stdint.h>
#include <stdbool.h>

#include <tree234.h>

#include "GenericCoeff/coeff.h"
#include "EvalExport_stub.h"

/*
 * polynomial term
 *
 * invariants:
 *   the power array is private to one instance of Term
 *   the coeff is private to one instance of Term
 */
typedef struct TERM
{
  Power * powers;
  /* the size of this array is given by (maxArity+1)
   * the first element is the monomial degree, ie the sum of the powers for all variables
   * variables are numbered from 0, thus one can say powers[var] to get the power of var
   */
  CoeffMutable coeff;
} Term;

#define MONOMIAL_DEGREE(powers) (powers[0])
#define POWER_OF_VAR(powers,var) (powers[var + 1])
#define SIZEOF_POWERS(arity) (sizeof(Power) * (arity+1))

/*
 * polynomial
 *
 * invariants:
 *   all terms are private to one instance of Poly,
 *   no aliasing allowed internally nor externally
 *
 *   the constTerm and errorBound are private to one instance of Poly
 *
 *   (Note that coefficients of equal value will typically be given
 *   the same stable pointer by ghc but these pointers are reference
 *   counted so that they could be safely treated as independent clones of
 *   the same immutable value.)
 */
typedef struct POLY
{
  Var maxArity; // nominal number of variables (0 < maxArity < 2^32)
  Size maxSize; // maximal number of non-constant terms (0 < maxSize < 2^10)
  Power maxDeg; // maximal degree of a term (ie sum of powers for all variables) (0 <= maxPow < 2^8)
  Size psize; // actual number of non-constant terms (0<=psize<=maxSize)
  CoeffMutable constTerm;
  CoeffMutable errorBound;
  // if non-zero, this structure represents a function enclosure
  // centred around the polynomial with radius errorBound in the max norm
  Term * terms;
} Poly;

/**
 * Print the internal representation of a polynomial to the console.
 * Mainly intended for debugging and checking aliasing.
 */
void
ADD_COEFF_CODE(printPoly)(Poly *p);

/**
 * Deallocate a polynomial including all its terms, power arrays and coefficients.
 */
void
ADD_COEFF_CODE(freePoly)(Poly *p);

/**
 * Apply the given Haskell function to all coefficients, including the errorBound.
 * The old coefficients are also deallocated.
 */
void
ADD_COEFF_CODE(mapCoeffsInPlace)(ConversionOp convert, Poly *p);

/*
 * preconditions:
 * no other persistent references to c and errorBound
 *   so that there is no coefficient aliasing
 */
Poly *
ADD_COEFF_CODE(newConstPoly)(CoeffMutable c, CoeffMutable errorBound,
    Var maxArity, Size maxSize, Power maxDeg);

/*
 * preconditions:
 * 0 <= var && var < maxArity
 * 0 < maxSize
 * no persistent references to zero, one and errorBound
 *   so that there is no coefficient aliasing
 */
Poly *
ADD_COEFF_CODE(newProjectionPoly)(CoeffMutable zero, CoeffMutable one,
    CoeffMutable errorBound, Var var, Var maxArity, Size maxSize, Power maxDeg);

///*
// * preconditions:
// * res has to point to an allocated polynomial with initialised maxArity and maxSize;
// * res has to have maxArity larger than p;
// * res has to have maxSize not smaller than p;
// */
//void
//incrementArity(Poly * res, Poly *p, Var * old2new);

Power
ADD_COEFF_CODE(getPowersDegree)(Power powers[], Var arity);

/*
 * The following operations expect all polynomial parameters and result space to
 * have matching maxArities.
 *
 * All Coeff parameters passed with a call are deallocated during the call.
 */

void
ADD_COEFF_CODE(addUp)(Coeff zero, ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addDn)(Coeff zero, ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addEncl)(ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2);

//void
//testAssign(Coeff sample, UnaryOpMutable assign, CoeffMutable to,
//    CoeffMutable from);

typedef void * Value; // A Haskell value passed via StablePtr

/*
 * Interpret the terms as Chebyshev polynomials and evaluate them for the
 * given values assigned to variables using the given addition and multiplication
 * Haskell operations.
 */
Value
ADD_COEFF_CODE(evalAtPtChebBasis)(Poly * p, Value * values, Value one,
    BinaryOp add, BinaryOp subtr, BinaryOp mult, ConversionOp cf2val);

/*
 * Interpret the terms as ordinary powers and evaluate them for the
 * given values assigned to variables using the given addition and multiplication
 * Haskell operations.
 */
Value
ADD_COEFF_CODE(evalAtPtPowerBasis)(Poly * p, Value * values, Value one,
    BinaryOp add, BinaryOp mult, ConversionOp cf2val);

/*
 *  Upper bound for a polynomial ignoring errorBound
 *  WARNING : does not check for thinness
 */
void
ADD_COEFF_CODE(boundUpThin)(Ops_Mutable * ops, CoeffMutable res, Poly * p);

/*
 *  Lower bound for a polynomial ignoring errorBound
 *  WARNING : does not check for thinness
 */

void
ADD_COEFF_CODE(boundDnThin)(Ops_Mutable * ops, CoeffMutable res, Poly * p);

/*
 *  Upper bound for a polynomial (centred enclosure)
 *  WARNING : assumes errorBound >= 0
 */

void
ADD_COEFF_CODE(boundUp)(Ops_Mutable * ops, CoeffMutable res, Poly * p);

/*
 *  Lower bound for a polynomial (centred enclosure)
 *  WARNING : assumes errorBound >= 0
 */

void
ADD_COEFF_CODE(boundDn)(Ops_Mutable * ops, CoeffMutable res, Poly * p);

void
ADD_COEFF_CODE(scaleUpThin)(Coeff zero, Ops_Mutable * ops,
    CoeffMutable c, Poly * p);

void
ADD_COEFF_CODE(scaleDnThin)(Coeff zero, Ops_Mutable * ops,
    CoeffMutable c, Poly * p);

void
ADD_COEFF_CODE(scaleEncl)(Ops_Mutable * ops, CoeffMutable c,
    Poly * p);

/*
 * Reduce maxDegree of p in place, collapsing higher degree terms into errorTerm
 */
void
ADD_COEFF_CODE(reduceDegreeEncl)(Ops_Mutable * ops,
    Power maxDeg, Poly * p);

void
ADD_COEFF_CODE(copySameSizes)(Ops_Mutable * ops, Poly * res, Poly * src);

void
ADD_COEFF_CODE(copyEncl)(ComparisonOp compare,
    Ops_Mutable * ops, Poly * res, Poly * src);

void
ADD_COEFF_CODE(copyUpThin)(ComparisonOp compare, Coeff zero,
    Ops_Mutable * ops, Poly * res, Poly * src);

void
ADD_COEFF_CODE(copyDnThin)(ComparisonOp compare, Coeff zero,
    Ops_Mutable * ops, Poly * res, Poly * src);

//void
//ADD_COEFF_CODE(reduceDegreeEnclUsingPureOps)(Ops_Pure * ops, Power maxDeg, Poly * p);

#endif /* POLY_H_ */
