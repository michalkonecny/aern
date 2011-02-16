#ifndef POLY_H_
#define POLY_H_

#include <stdint.h>

#include "GenericCoeff/coeff.h"
#include "EvalExport_stub.h"

/*
 * polynomial term
 *
 * invariants:
 *   the power array is private to one instance of Term
 *
 *   the coeff is private to one instance of Term
 */
typedef struct TERM
{
  Power * powers;
  Coeff coeff;
} Term;

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
  Coeff constTerm;
  Coeff errorBound;
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
ADD_COEFF_CODE(newConstPoly)(Coeff c, Coeff errorBound, Var maxArity,
    Size maxSize, Power maxDeg);

/*
 * preconditions:
 * 0 <= var && var < maxArity
 * 0 < maxSize
 * no persistent references to zero, one and errorBound
 *   so that there is no coefficient aliasing
 */
Poly *
ADD_COEFF_CODE(newProjectionPoly)(Coeff zero, Coeff one, Coeff errorBound,
    Var var, Var maxArity, Size maxSize, Power maxDeg);

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
ADD_COEFF_CODE(addUpUsingPureOps)(Coeff zero, ComparisonOp compare,
    Ops_Pure * ops, Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addDnUsingPureOps)(Coeff zero, ComparisonOp compare,
    Ops_Pure * ops, Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addEnclUsingPureOps)(ComparisonOp compare, Ops_Pure * ops,
    Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addUpUsingMutableOps)(Coeff zero, ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addDnUsingMutableOps)(Coeff zero, ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2);

void
ADD_COEFF_CODE(addEnclUsingMutableOps)(ComparisonOp compare,
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
 * Upper bound for sum of absolute values of coefficients
 * DEALLOCATES : zero
 */

Coeff
ADD_COEFF_CODE(sumUpAbsCoeffs)(Coeff zero, Ops_Pure * ops, Poly * p);

/*
 *  Upper bound for a polynomial ignoring errorBound
 *  WARNING : does not check for thinness
 */

Coeff
ADD_COEFF_CODE(boundUpThin)(Ops_Pure * ops, Poly * p);

/*
 *  Lower bound for a polynomial ignoring errorBound
 *  WARNING : does not check for thinness
 */

Coeff
ADD_COEFF_CODE(boundDnThin)(Ops_Pure * ops, Poly * p);

/*
 *  Upper bound for a polynomial (centred enclosure)
 *  WARNING : assumes errorBound >= 0
 */

Coeff
ADD_COEFF_CODE(boundUp)(Ops_Pure * ops, Poly * p);

/*
 *  Lower bound for a polynomial (centred enclosure)
 *  WARNING : assumes errorBound >= 0
 */

Coeff
ADD_COEFF_CODE(boundDn)(Ops_Pure * ops, Poly * p);

/*
 * Scale p in place by c rounding upwards, collapsing errorTerm into constTerm.
 */
void
ADD_COEFF_CODE(scaleUpThinUsingPureOps)(Coeff zero, Ops_Pure * ops, Coeff c, Poly * p);

/*
 * Scale p in place by c rounding downwards, collapsing errorTerm into constTerm.
 */
void
ADD_COEFF_CODE(scaleDnThinUsingPureOps)(Coeff zero, Ops_Pure * ops, Coeff c, Poly * p);

/*
 * Scale p in place by c while increasing errorTerm to take rounding into account.
 */
void
ADD_COEFF_CODE(scaleEnclUsingPureOps)(Ops_Pure * ops, Coeff c, Poly * p);

#endif /* POLY_H_ */
