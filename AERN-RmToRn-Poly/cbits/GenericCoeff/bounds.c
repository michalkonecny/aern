/*
 * GenericCoeff/bounds.c
 *
 *  Created on: 3 Feb 2011
 *     Authors: Jan Duracz, Michal Konecny
 */

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"

void
ADD_COEFF_CODE(sumUpAbsCoeffs)(Ops_Mutable * ops, CoeffMutable res, Poly * p)
{
  Term * terms = p -> terms;
  Size pSize = p -> psize;

  CoeffMutable temp = CFM_NEW(ops, ops -> sample);

  int i = 0;
  while (i < pSize)
    {
      CFM_ABS_UP(ops, temp, terms[i].coeff); // temp := terms[i].coeff;
      CFM_ADD_UP(ops, res, res, temp); // res += temp;
      i++;
    }

  CFM_FREE(temp);
}

void
ADD_COEFF_CODE(boundUpThin)(Ops_Mutable * ops, CoeffMutable res, Poly * p)
{
  CFM_ASSIGN(ops, res, p -> constTerm);
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
}

void
ADD_COEFF_CODE(boundDnThin)(Ops_Mutable * ops, CoeffMutable res, Poly * p)
{
  CFM_ASSIGN(ops, res, p -> constTerm);
  CFM_NEG(ops, res, res); // res := - constTerm
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
  CFM_NEG(ops, res, res); // res := - res
}

void
ADD_COEFF_CODE(boundUp)(Ops_Mutable * ops, CoeffMutable res, Poly * p)
{
  CFM_ASSIGN(ops, res, p -> constTerm);
  CFM_ADD_UP(ops, res, res, p -> errorBound);
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
}

void
ADD_COEFF_CODE(boundDn)(Ops_Mutable * ops, CoeffMutable res, Poly * p)
{
  CFM_ASSIGN(ops, res, p -> constTerm);
  CFM_SUB_DN(ops, res, res, p -> errorBound);
  CFM_NEG(ops, res, res); // res := - constTerm
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
  CFM_NEG(ops, res, res); // res := - res
}
