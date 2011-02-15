/*
 * multiplication.c
 *
 *  Created on: 15 Feb 2011
 *      Author: jas
 */

#include "DoubleCoeff/poly.h"

void
ADD_COEFF_CODE(scaleTerms)(Ops_Pure * ops, Coeff c, Poly * p)
{
  Term * terms = p -> terms;
  Var pSize = p -> psize;

  for (int i = 0; i < pSize; i++)
  {
	Coeff oldCoeff = terms[i].coeff;
	Coeff coeffScaledUp = CF_MUL_UP(ops, c, oldCoeff);
	Coeff coeffScaledDn = CF_MUL_DN(ops, c, oldCoeff);
	CF_FREE(oldCoeff);
	terms[i].coeff = coeffScaledUp; // scale the coefficient
	Coeff maxError = CF_SUB_UP(ops, coeffScaledUp, coeffScaledDn); // >= 0
	CF_FREE(coeffScaledUp);
	CF_FREE(coeffScaledDn);
	Coeff oldErrorBound = p -> errorBound;
	p -> errorBound = CF_ADD_UP(ops, oldErrorBound, maxError); // accumulate the error
	CF_FREE(oldErrorBound);
	CF_FREE(maxError);
  }
}

void
ADD_COEFF_CODE(scaleEncl)(Ops_Pure * ops, Coeff c, Poly * p)
{
  ADD_COEFF_CODE(scaleTerms)(ops, c, p); // scale non-constant terms

  Coeff oldConstTerm = p -> constTerm;
  Coeff constTermScaledUp = CF_MUL_UP(ops, c, oldConstTerm);
  Coeff constTermScaledDn = CF_MUL_DN(ops, c, oldConstTerm);
  CF_FREE(oldConstTerm);
  p -> constTerm = constTermScaledUp; // scale the constant term
  Coeff maxError = CF_SUB_UP(ops, constTermScaledUp, constTermScaledDn);
  CF_FREE(constTermScaledUp);
  CF_FREE(constTermScaledDn);
  Coeff oldErrorBound = p -> errorBound;
  p -> errorBound = CF_ADD_UP(ops, oldErrorBound, maxError); // increase errorBound
  CF_FREE(oldErrorBound);
  CF_FREE(maxError);
}

