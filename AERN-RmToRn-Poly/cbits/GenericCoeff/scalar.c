/*
 * scalar.c
 *
 *  Created on: 15 Feb 2011
 *      Author: jas
 */

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"

void
ADD_COEFF_CODE(scaleTermsUsingMutableOps)(Ops_Mutable * ops, Coeff c, Poly * p)
{
  Var pSize = p -> psize;
  if (pSize > 0) // any terms to scale?
  {
    Term * terms = p -> terms;

    Coeff oldCoeff;
    Coeff coeffScaledUp;
    Coeff coeffScaledDn;
    Coeff maxError;
    Coeff errorBound;
    for (int i = 0; i < pSize; i++)
    {
      CFM_ASSIGN_VAL(ops, oldCoeff, terms[i].coeff); // fetch coefficient value
      CFM_MUL_UP(ops, coeffScaledUp, c, oldCoeff); // scale coefficient
      CFM_MUL_DN(ops, coeffScaledDn, c, oldCoeff);
      CFM_ASSIGN_VAL(ops, terms[i].coeff, coeffScaledUp); // update coefficient
      // note that we choose the upward rounded scaled constant term, can cause upward drift
      CFM_SUB_UP(ops, maxError, coeffScaledUp, coeffScaledDn); // bound rounding error
      CFM_ADD_UP(ops, errorBound, errorBound, maxError); // accumulate rounding error
    }
    CFM_FREE(oldCoeff);
    CFM_FREE(coeffScaledUp);
    CFM_FREE(coeffScaledDn);
    CFM_FREE(maxError);
    CFM_ASSIGN_VAL(ops, p -> errorBound, errorBound); // account for rounding errors
    CFM_FREE(errorBound);
  }
}

void
ADD_COEFF_CODE(scaleUpThinUsingMutableOps)(Coeff zero, Ops_Mutable * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingMutableOps)(ops, c, p); // scale non-constant terms
  Coeff constTerm;
  CFM_MUL_UP(ops, constTerm, c, p -> constTerm); // scale constTerm up
  CFM_ADD_UP(ops, p -> constTerm, constTerm, p -> errorBound); // account for errorBound
  CFM_FREE(constTerm);
  CFM_ASSIGN_VAL(ops, p -> errorBound, zero); // collapse errorBound
}

void
ADD_COEFF_CODE(scaleDnThinUsingMutableOps)(Coeff zero, Ops_Mutable * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingMutableOps)(ops, c, p); // scale non-constant terms
  Coeff constTerm;
  CFM_MUL_DN(ops, constTerm, c, p -> constTerm); // scale constTerm down
  CFM_SUB_DN(ops, p -> constTerm, constTerm, p -> errorBound); // account for errorBound
  CFM_FREE(constTerm);
  CFM_ASSIGN_VAL(ops, p -> errorBound, zero); // collapse errorBound
}

void
ADD_COEFF_CODE(scaleEnclUsingMutableOps)(Ops_Mutable * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingMutableOps)(ops, c, p); // scale non-constant terms
  Coeff oldConstTerm;
  Coeff constTermScaledUp;
  Coeff constTermScaledDn;
  Coeff maxError;
  CFM_ASSIGN_VAL(ops, oldConstTerm, p -> constTerm); // fetch constTerm value
  CFM_MUL_UP(ops, constTermScaledUp, c, oldConstTerm); // scale constTerm
  CFM_MUL_DN(ops, constTermScaledDn, c, oldConstTerm);
  CFM_FREE(oldConstTerm);
  CFM_ASSIGN_VAL(ops, p -> constTerm, constTermScaledUp); // update constTerm
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

void
ADD_COEFF_CODE(scaleUpThinUsingPureOps)(Coeff zero, Ops_Pure * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingPureOps)(ops, c, p); // scale term coefficients
  Coeff oldConstTerm = p -> constTerm;
  Coeff constTermScaledUp = CF_MUL_UP(ops, c, oldConstTerm); // scale constTerm up
  CF_FREE(oldConstTerm);
  Coeff oldErrorBound = p -> errorBound;
  p -> constTerm = CF_ADD_UP(ops, constTermScaledUp, oldErrorBound); // account for errorBound
  CF_FREE(constTermScaledUp);
  CF_FREE(oldErrorBound);
  p -> errorBound = zero; // collapse errorBound
}

void
ADD_COEFF_CODE(scaleDnThinUsingPureOps)(Coeff zero, Ops_Pure * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingPureOps)(ops, c, p); // scale term coefficients
  Coeff oldConstTerm = p -> constTerm;
  Coeff constTermScaledDn = CF_MUL_DN(ops, c, oldConstTerm); // scale constTerm down
  CF_FREE(oldConstTerm);
  Coeff oldErrorBound = p -> errorBound;
  p -> constTerm = CF_SUB_DN(ops, constTermScaledDn, oldErrorBound); // account for errorBound
  CF_FREE(constTermScaledDn);
  CF_FREE(oldErrorBound);
  p -> errorBound = zero; // collapse errorBound
}

void
ADD_COEFF_CODE(scaleEnclUsingPureOps)(Ops_Pure * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTermsUsingPureOps)(ops, c, p); // scale non-constant terms

  Coeff oldConstTerm = p -> constTerm;
  Coeff constTermScaledUp = CF_MUL_UP(ops, c, oldConstTerm);
  Coeff constTermScaledDn = CF_MUL_DN(ops, c, oldConstTerm);
  CF_FREE(oldConstTerm);
  p -> constTerm = constTermScaledUp; // scale the constant term
  // note that we choose the upward rounded scaled constant term, can cause upward drift
  Coeff maxError = CF_SUB_UP(ops, constTermScaledUp, constTermScaledDn);
  CF_FREE(constTermScaledUp);
  CF_FREE(constTermScaledDn);
  Coeff oldErrorBound = p -> errorBound;
  p -> errorBound = CF_ADD_UP(ops, oldErrorBound, maxError); // account for rounding error
  CF_FREE(oldErrorBound);
  CF_FREE(maxError);
}

