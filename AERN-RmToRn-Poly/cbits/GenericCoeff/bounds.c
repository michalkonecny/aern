/*
 * GenericCoeff/bounds.c
 *
 *  Created on: 3 Feb 2011
 *      Author: jan
 */

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"

Coeff
ADD_COEFF_CODE(sumUpAbsCoeffs)(Coeff zero, Ops_Pure * ops, Poly * p)
{
	Coeff res = zero;
	Term * terms = p -> terms;
	Size pSize = p -> psize;
	int i = 0;
	while (i < pSize) {
		Coeff absCoeff = CF_ABS_UP(ops, terms[i].coeff);
		Coeff oldRes = res;
		res = CF_ADD_UP(ops, res, absCoeff);
		CF_FREE(absCoeff);
		CF_FREE(oldRes);
		i++;
	}
	return res;
}

Coeff
ADD_COEFF_CODE(boundUpThin)(Ops_Pure * ops, Poly * p)
{
	Coeff constTerm = CF_CLONE(p -> constTerm);
	return ADD_COEFF_CODE(sumUpAbsCoeffs)(constTerm, ops, p);
	// no need to free constTerm as sumUpAbsCoeffs does this
}

Coeff
ADD_COEFF_CODE(boundDnThin)(Ops_Pure * ops, Poly * p)
{
	Coeff negConstTerm = CF_NEG(ops, p -> constTerm);
	Coeff res = ADD_COEFF_CODE(sumUpAbsCoeffs)(negConstTerm, ops, p);
	// no need to free negConstTerm as sumUpAbsCoeffs does this
	Coeff oldRes = res;
	res = CF_NEG(ops, res);
	CF_FREE(oldRes);
	return res;
}

Coeff
ADD_COEFF_CODE(boundUp)(Ops_Pure * ops, Poly * p)
{
	Coeff constPlusError = CF_ADD_UP(ops, p -> constTerm, p -> errorBound);
	return ADD_COEFF_CODE(sumUpAbsCoeffs)(constPlusError, ops, p);
	// no need to free constPlusError as sumUpAbsCoeffs does this
}

Coeff
ADD_COEFF_CODE(boundDn)(Ops_Pure * ops, Poly * p)
{
	Coeff constMinusError = CF_SUB_DN(ops, p -> constTerm, p -> errorBound);
	Coeff negConstMinusError = CF_NEG(ops, constMinusError);
	CF_FREE(constMinusError);
	Coeff res = ADD_COEFF_CODE(sumUpAbsCoeffs)(negConstMinusError, ops, p);
	// no need to free negConstMinusError as sumUpAbsCoeffs does this
	Coeff oldRes = res;
	res = CF_NEG(ops, res);
	CF_FREE(oldRes);
	return res;
}
