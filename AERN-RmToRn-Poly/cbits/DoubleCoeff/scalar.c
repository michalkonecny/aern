/*
 * This file should differ from its GenericCoeff analogue
 * only in the following include line and by omitting
 * the ...UsingPureOps functions.
 */
#include "DoubleCoeff/coeff.h"
#include "DoubleCoeff/poly.h"

void
ADD_COEFF_CODE(scaleTermsUsingMutableOps)(Ops_Mutable * ops, CoeffMutable c, Poly * p)
{
  Var pSize = p -> psize;
  if (pSize > 0) // any terms to scale?
  {
    Term * terms = p -> terms;
    CoeffMutable coeffScaledDn = CFM_NEW(ops, CFM_SAMPLE(ops));
    CoeffMutable maxError = CFM_NEW(ops, CFM_SAMPLE(ops));
    CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops));
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

void
ADD_COEFF_CODE(scaleUpThinUsingMutableOps)(Coeff zero, Ops_Mutable * ops, CoeffMutable c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingMutableOps)(ops, c, p); // scale non-constant terms
  CoeffMutable constTerm = CFM_NEW(ops, CFM_SAMPLE(ops));
  CFM_MUL_UP(ops, constTerm, c, p -> constTerm); // scale constTerm up
  CFM_ADD_UP(ops, p -> constTerm, constTerm, p -> errorBound); // account for errorBound
  CFM_FREE(constTerm);
  CFM_ASSIGN_VAL(ops, p -> errorBound, zero); // collapse errorBound
}

void
ADD_COEFF_CODE(scaleDnThinUsingMutableOps)(Coeff zero, Ops_Mutable * ops, CoeffMutable c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingMutableOps)(ops, c, p); // scale non-constant terms
  CoeffMutable constTerm = CFM_NEW(ops, CFM_SAMPLE(ops));
  CFM_MUL_DN(ops, constTerm, c, p -> constTerm); // scale constTerm down
  CFM_SUB_DN(ops, p -> constTerm, constTerm, p -> errorBound); // account for errorBound
  CFM_FREE(constTerm);
  CFM_ASSIGN_VAL(ops, p -> errorBound, zero); // collapse errorBound
}

void
ADD_COEFF_CODE(scaleEnclUsingMutableOps)(Ops_Mutable * ops, CoeffMutable c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingMutableOps)(ops, c, p); // scale non-constant terms
  CoeffMutable oldConstTerm = CFM_NEW(ops, CFM_SAMPLE(ops));
  CoeffMutable constTermScaledUp = CFM_NEW(ops, CFM_SAMPLE(ops));
  CoeffMutable constTermScaledDn = CFM_NEW(ops, CFM_SAMPLE(ops));
  CoeffMutable maxError = CFM_NEW(ops, CFM_SAMPLE(ops));
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

void
ADD_COEFF_CODE(scaleTermsUsingPureOps)(Ops_Pure * ops, Coeff c, Poly * p)
{
  Term * terms = p -> terms;
  Var pSize = p -> psize;

  for (int i = 0; i < pSize; i++)
  {
	Coeff oldCoeff = terms[i].coeff;
	Coeff coeffScaledUp = CF_MUL_UP(ops, c, oldCoeff);
	Coeff coeffScaledDn = CF_MUL_DN(ops, c, oldCoeff);
	CF_FREE(oldCoeff);
	terms[i].coeff = coeffScaledUp; // scale term coefficient
	Coeff maxError = CF_SUB_UP(ops, coeffScaledUp, coeffScaledDn); // bound rounding error
	CF_FREE(coeffScaledUp);
	CF_FREE(coeffScaledDn);
	Coeff oldErrorBound = p -> errorBound;
	p -> errorBound = CF_ADD_UP(ops, oldErrorBound, maxError); // accumulate the error
	CF_FREE(oldErrorBound);
	CF_FREE(maxError);
  }
}