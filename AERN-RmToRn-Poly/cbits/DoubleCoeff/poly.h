#ifndef POLY_H_
#define POLY_H_

#include <stdint.h>

/*
 * This file should differ from its GenericCoeff analogue
 * only in:
 *   * the following include line
 *   * by omitting the ...UsingMutableOps functions
 */
#include "DoubleCoeff/coeff.h"
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
 *   the constTerm is private to one instance of Poly
 */
typedef struct POLY
{
  Var maxArity; // nominal number of variables (0 < maxArity < 2^32)
  Size maxSize; // maximal number of non-constant terms (0 < maxSize < 2^10)
  Power maxDeg; // maximal degree of a term (ie sum of powers for all variables) (0 <= maxPow < 2^8)
  Size psize; // actual number of non-constant terms (0<=psize<=maxSize)
  Coeff constTerm;
  Term * terms;
} Poly;

void
ADD_COEFF_CODE(freePoly)(Poly *p);

//void
//mapCoeffsInPlace(ConversionOp convert, Poly *p);

/*
 * preconditions:
 * no persistent references to c and this pointer is unique for each call
 */
Poly *
ADD_COEFF_CODE(newConstPoly)(const Coeff c, Var maxArity, Size maxSize, Power maxDeg);

/*
 * preconditions:
 * 0 <= var && var < maxArity
 * 0 < maxSize
 * no persistent references to zero, one and these pointers are unique to each call
 */
Poly *
ADD_COEFF_CODE(newProjectionPoly)(const Coeff zero, const Coeff one, Var var,
    Var maxArity, Size maxSize, Power maxDeg);

///*
// * preconditions:
// * res has to point to an allocated polynomial with initialised maxArity and maxSize;
// * res has to have maxArity larger than p;
// * res has to have maxSize not smaller than p;
// */
//void
//incrementArity(Poly * res, Poly *p, Var * old2new);

/*
 * The following operations expect all polynomial parameters and result space to
 * have matching maxArities.
 *
 * All Coeff parameters passed with a call are deallocated during the call.
 */

void
ADD_COEFF_CODE(addUpUsingPureOps)(Coeff zero, ComparisonOp compare,
    const Ops_Pure * ops, Poly *res, const Poly * p1, const Poly * p2);

void
ADD_COEFF_CODE(addDnUsingPureOps)(Coeff zero, ComparisonOp compare,
    const Ops_Pure * ops, Poly *res, const Poly * p1, const Poly * p2);

typedef void * Value; // A Haskell value passed via StablePtr

/*
 * Interpret the terms as Chebyshev polynomials and evaluate them for the
 * given values assigned to variables using the given addition and multiplication
 * Haskell operations.
 */
Value
ADD_COEFF_CODE(evalAtPtChebBasis)(const Poly *, const Value *, const Value,
    const BinaryOp, const BinaryOp, const BinaryOp, const ConversionOp);

#endif /* POLY_H_ */
