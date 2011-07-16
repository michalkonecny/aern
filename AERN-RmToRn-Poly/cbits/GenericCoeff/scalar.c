/*
 * scalar.c
 *
 *  Created on: 15 Feb 2011
 *      Author: jas
 */

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"

void
ADD_COEFF_CODE(addConstUpThin)(Ops * ops, CoeffMutable c, Poly * p)
{
  CFM_ADD_UP(ops, p -> constTerm, p -> constTerm, c);
}

void
ADD_COEFF_CODE(addConstDnThin)(Ops * ops, CoeffMutable c, Poly * p)
{
  CFM_ADD_DN(ops, p -> constTerm, p -> constTerm, c);
}

void
ADD_COEFF_CODE(addConstEncl)(Ops * ops, CoeffMutable c, Poly * p)
{
  CoeffMutable temp = CFM_NEW(ops, CFM_ZERO(ops));

  CFM_ADD_UP(ops, temp, p -> constTerm, c);
  CFM_ADD_DN(ops, p -> constTerm, p -> constTerm, c);
  // it does not matter which of the above two results is set to constTerm

  CFM_SUB_UP(ops, temp, temp, p -> constTerm);
  CFM_ADD_UP(ops, p -> errorBound, p -> errorBound, temp);

  CFM_FREE(temp);
}

void
ADD_COEFF_CODE(scaleTerms)(Ops * ops, CoeffMutable c, Poly * p)
{
  Var pSize = p -> psize;
  // is c zero?
  if (CFM_IS_EXACTZERO(ops, c))
    {
      CFM_ASSIGN_VAL(ops, p -> constTerm, CFM_ZERO(ops));
      CFM_ASSIGN_VAL(ops, p -> errorBound, CFM_ZERO(ops));
      // remove all terms:
      for (int i = 0; i < pSize; ++i)
        {
          CFM_FREE(p -> terms[i].coeff);
        }
      p -> psize = 0;
    }
  else
    {
      if (pSize > 0) // any terms to scale?

        {
          Term * terms = p -> terms;
          CoeffMutable coeffScaledDn = CFM_NEW(ops, CFM_ZERO(ops));
          CoeffMutable maxError = CFM_NEW(ops, CFM_ZERO(ops));
          CoeffMutable errorBound = CFM_NEW(ops, CFM_ZERO(ops));
          for (int i = 0; i < pSize; i++)
            {
              CFM_MUL_DN(ops, coeffScaledDn, c, terms[i].coeff);
              CFM_MUL_UP(ops, terms[i].coeff, c, terms[i].coeff); // scale coefficient
              // note that we choose the upward rounded scaled constant term, can cause upward drift
              CFM_SUB_UP(ops, maxError, terms[i].coeff, coeffScaledDn); // bound rounding error
              CFM_ADD_UP(ops, errorBound, errorBound, maxError); // accumulate rounding error
            }
          CFM_FREE(coeffScaledDn);
          CFM_FREE(maxError);
          CFM_ASSIGN(ops, p -> errorBound, errorBound); // account for rounding errors
          CFM_FREE(errorBound);
        }
    }
}

void
ADD_COEFF_CODE(scaleUpThin)(Ops * ops, CoeffMutable c, Poly * p)
{
  ADD_COEFF_CODE(scaleTerms)(ops, c, p); // scale non-constant terms
  CoeffMutable constTerm = CFM_NEW(ops, CFM_ZERO(ops));
  CFM_MUL_UP(ops, constTerm, c, p -> constTerm); // scale constTerm up
  CFM_ADD_UP(ops, p -> constTerm, constTerm, p -> errorBound); // account for errorBound
  CFM_FREE(constTerm);
  CFM_ASSIGN_VAL(ops, p -> errorBound, CFM_ZERO(ops)); // collapse errorBound
}

void
ADD_COEFF_CODE(scaleDnThin)(Ops * ops, CoeffMutable c, Poly * p)
{
  ADD_COEFF_CODE(scaleTerms)(ops, c, p); // scale non-constant terms
  CoeffMutable constTerm = CFM_NEW(ops, CFM_ZERO(ops));
  CFM_MUL_DN(ops, constTerm, c, p -> constTerm); // scale constTerm down
  CFM_SUB_DN(ops, p -> constTerm, constTerm, p -> errorBound); // account for errorBound
  CFM_FREE(constTerm);
  CFM_ASSIGN_VAL(ops, p -> errorBound, CFM_ZERO(ops)); // collapse errorBound
}

void
ADD_COEFF_CODE(scaleEncl)(Ops * ops, CoeffMutable c, Poly * p)
{
  ADD_COEFF_CODE(scaleTerms)(ops, c, p); // scale non-constant terms
  CoeffMutable oldConstTerm = CFM_NEW(ops, CFM_ZERO(ops));
  CoeffMutable constTermScaledUp = CFM_NEW(ops, CFM_ZERO(ops));
  CoeffMutable constTermScaledDn = CFM_NEW(ops, CFM_ZERO(ops));
  CoeffMutable maxError = CFM_NEW(ops, CFM_ZERO(ops));
  CFM_ASSIGN(ops, oldConstTerm, p -> constTerm); // fetch constTerm value
  CFM_MUL_UP(ops, constTermScaledUp, c, oldConstTerm); // scale constTerm
  CFM_MUL_DN(ops, constTermScaledDn, c, oldConstTerm);
  CFM_FREE(oldConstTerm);
  CFM_ASSIGN(ops, p -> constTerm, constTermScaledUp); // update constTerm
  CFM_SUB_UP(ops, maxError, constTermScaledUp, constTermScaledDn); // bound rounding error
  CFM_FREE(constTermScaledUp);
  CFM_FREE(constTermScaledDn);
  CFM_ADD_UP(ops, p -> errorBound, p -> errorBound, maxError); // account for rounding error
  CFM_FREE(maxError);
}
