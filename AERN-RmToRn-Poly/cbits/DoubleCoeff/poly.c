#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * This file should differ from its GenericCoeff analogue
 * only in the following two include lines
 * and by omitting methods: printPoly, mapCoeffsInPlace.
 */
#include "DoubleCoeff/coeff.h"
#include "DoubleCoeff/poly.h"
#include "EvalExport_stub.h"

void
ADD_COEFF_CODE(freePoly)(Poly *p)
{
//  printf("freePoly: starting\n");
  // free the Poly struct:
  Size maxSize = p -> maxSize;
  Size psize = p -> psize;
  Term * terms = p -> terms;
  CF_FREE(p -> constTerm);
  CF_FREE(p -> errorBound);
  free(p);

  Size i = 0;
  // free the power arrays:
  while( i < psize )
    {
      free(terms[i].powers);
      CF_FREE(terms[i].coeff);
      i++;
    }

  // free the coeffs:
  while( i < maxSize )
    {
      free(terms[i].powers);
      i++;
    }

  // free the terms array:
  free(terms);
//  printf("freePoly: finished\n");
}

Poly *
ADD_COEFF_CODE(newConstPoly)(Coeff c, Coeff errorBound, Var maxArity,
    Size maxSize, Power maxDeg)
{
  //  printf("newConstPoly: starting\n");
  Poly * poly = (Poly *) malloc(sizeof(Poly));
  poly -> maxArity = maxArity;
  poly -> maxSize = maxSize;
  poly -> maxDeg = maxDeg;
  poly -> errorBound = errorBound;
  poly -> psize = 0;
  poly -> constTerm = c;
  poly -> terms = malloc(maxSize * sizeof(Term));

  // allocate space for terms' powers:
  for (Size i = 0; i < maxSize; i++)
    {
      (poly -> terms)[i].powers = (Power *) malloc(SIZEOF_POWERS(maxArity));
      // no need to initialise powers and
      // coefficients as these terms are inactive
    }

  //  printf("newConstPoly: returning\n");
  return poly;
}

Poly *
ADD_COEFF_CODE(newProjectionPoly)(Coeff zero, Coeff one, Coeff errorBound,
    Var var, Var maxArity, Size maxSize, Power maxDeg)
{
  Poly * poly = ADD_COEFF_CODE(newConstPoly)(zero, errorBound, maxArity,
      maxSize, maxDeg);

  // add one term for the variable:
  poly -> psize = 1;
  Term * term = poly -> terms;

  // initialise the term's coeff:
  term -> coeff = one;
  //  printf("newProjectionPoly: coeff one address = %p\n", term -> coeff);

  // initialise the term's powers:
  Power * powers = term -> powers;

  // initialise the "cache" of the monomial degree:
  MONOMIAL_DEGREE(powers) = 1;

  // all zero:
  FOREACH_VAR_ARITY(var,maxArity)
    {
      POWER_OF_VAR(powers,var) = 0;
    }

  // except the chosen var:
  POWER_OF_VAR(powers,var) = 1;

  return poly;
}
