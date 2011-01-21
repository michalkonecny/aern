#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

void
ADD_COEFF_CODE(freePoly)(Poly *p)
{
  // free the Poly struct:
  Size maxSize = p -> maxSize;
  Term * terms = p -> terms;
  CF_FREE(p -> constTerm);
  free(p);

  // free the power arrays and coeffs:
  for (Size i = 0; i < maxSize; i++)
    {
      free(terms[i].powers);
      CF_FREE(terms[i].coeff);
    }

  // free the terms array:
  free(terms);
}

void
ADD_COEFF_CODE(mapCoeffsInPlace)(ConversionOp convert, Poly *p)
{
  p -> constTerm = CF_CONVERT(convert, p -> constTerm);
  Size psize = p -> psize;
  Term * terms = p -> terms;
  for (Size i = 0; i < psize; ++i)
    {
      terms[i].coeff = CF_CONVERT(convert, terms[i].coeff);
    }
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
      (poly -> terms)[i].powers = (Power *) malloc(sizeof(Power) * maxArity);
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

  // all zero:
  for (Var i = 0; i < maxArity; ++i)
    {
      powers[i] = 0;
    }
  // except the chosen var:
  powers[var] = 1;

  return poly;
}

Power
ADD_COEFF_CODE(getPowersDegree)(Power powers[], Var arity)
{
  Power result = 0;

  for (int i = 0; i < arity; i++)
    {
      result += powers[i];
    }

  return result;
}
