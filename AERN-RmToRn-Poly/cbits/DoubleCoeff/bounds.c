/*
 * bounds.c
 *
 *  Created on: 3 Feb 2011
 *      Author: jan
 */

#include "DoubleCoeff/coeff.h"
#include "DoubleCoeff/poly.h"

/*
 *  Upper bound for sum of absolute values of coefficients
 */

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

/*
 *  Upper bound for a polynomial ignoring errorBound
 *
 *  WARNING : does not check for thinness
 */

Coeff
ADD_COEFF_CODE(boundUpThin)(Ops_Pure * ops, Poly * p)
{
	return ADD_COEFF_CODE(sumUpAbsCoeffs)(p -> constTerm, ops, p);
}

/*
 *  Lower bound for a polynomial ignoring errorBound
 *
 *  WARNING : does not check for thinness
 */

Coeff
ADD_COEFF_CODE(boundDnThin)(Ops_Pure * ops, Poly * p)
{
	return -ADD_COEFF_CODE(sumUpAbsCoeffs)(-(p -> constTerm), ops, p);
}

/*
 *  Upper bound for a polynomial (centred enclosure)
 *
 *  WARNING : assumes errorBound >= 0
 */

Coeff
ADD_COEFF_CODE(boundUp)(Ops_Pure * ops, Poly * p)
{
	Coeff res = CF_ADD_UP(ops, p -> constTerm, p -> errorBound);
	Term * terms = p -> terms;
	Size pSize = p -> psize;
	int i = 0;
	while (i < pSize) {
		res = CF_ADD_UP(ops, res, CF_ABS_UP(ops, terms[i].coeff));
		i++;
	}
	return res;
}

/*
 *  Lower bound for a polynomial (centred enclosure)
 *
 *  WARNING : assumes errorBound >= 0
 */

Coeff
ADD_COEFF_CODE(boundDn)(Ops_Pure * ops, Poly * p)
{
	Coeff res = CF_SUB_DN(ops, p -> constTerm, p -> errorBound);
	Term * terms = p -> terms;
	Size pSize = p -> psize;
	int i = 0;
	while (i < pSize) {
		res = CF_SUB_DN(ops, res, CF_ABS_DN(ops, terms[i].coeff));
		i++;
	}
	return res;
}



