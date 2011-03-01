#include <string.h>

#include <GenericCoeff/coeff.h>
#include <GenericCoeff/poly.h>

/*
 * ASSUMES: deg <= maxDeg && size <= resSize
 */
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops_Mutable * ops, Var arity,
    Poly * res, Term * resTerms, Size resSize,
    Poly * src, Term * terms, Size size)
{
  int i = 0; // save allocating and initialising one int by using two whiles
  while (i < size) // for each term in src
  {
    CFM_ASSIGN(ops, resTerms[i].coeff, terms[i].coeff); // copy the coefficient
    memmove(resTerms[i].powers, terms[i].powers, SIZEOF_POWERS(arity)); // and powers
    i++;
  }
  while (i < resSize) // any terms left unassigned in res?
  {
    CFM_FREE(resTerms[i].coeff); // free their coefficients
  }
  res -> psize = size; // forget remaining terms in res
}

/*
 * ASSUMES: maxArity and maxSize of src and res are the same
 * INVARIANT : leaves maxDeg of res unchanged
 */

void
ADD_COEFF_CODE(copyEnclUsingMutableOps)(Ops_Mutable * ops, Poly * res, Poly * src)
{
  Var arity = src -> maxArity;
  Size size = src -> psize;
  Term * terms = src -> terms;
  Power deg = (size == 0) ? 0 : MONOMIAL_DEGREE(terms[size-1].powers);

  Size resSize = res -> psize;
  Power resMaxDeg = res -> maxDeg;
  Term * resTerms = res -> terms;

  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);

  if (deg <= resMaxDeg && size <= resSize) // just copy src terms into res?
  {
    ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, arity,
        res, resTerms, resSize,
        src, terms, size);
    CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
  }
  else // some reduction will be needed
  {
    CoeffMutable absError = CFM_NEW(ops, CFM_SAMPLE(ops)); // reduction error
    CoeffMutable errorBound = CFM_NEW(ops, src -> errorBound); // accumulator
    while (deg > resMaxDeg) // degree reduce i.e. truncate last term?
    {
      size--; // now size == index of truncated term
      CFM_ABS_UP(ops, absError, terms[size].coeff); // compute truncation error
      CFM_ADD_UP(ops, errorBound, errorBound, absError); // accumulate error
      deg = (size == 0) ? 0 : MONOMIAL_DEGREE(terms[size].powers); // update degree
    }
    if (size <= resSize) // just copy remaining src terms into res?
    {
      ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, arity,
          res, resTerms, resSize,
          src, terms, size);
      CFM_ASSIGN(ops, res -> errorBound, errorBound);
    }
//    else // some reduction will be needed
//    {
//
//    }
  }
}


































void
ADD_COEFF_CODE(reduceDegreeEnclUsingMutableOps)(Ops_Mutable * ops, Power maxDeg, Poly * p)
{
  Term * terms = p -> terms;
  Size oldSize = p -> psize;
  Size lastTermIndex = oldSize-1; // track last term for swapping
  CoeffMutable maxError = CFM_NEW(ops, CFM_SAMPLE(ops));
  CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops));
  for (int i = 0; i <= lastTermIndex; i++)
  {
    if (MONOMIAL_DEGREE(terms[i].powers) > maxDeg && lastTermIndex > 0) // reduce term?
    {
      CFM_ABS_UP(ops, maxError, terms[i].coeff); // compute reduction error
      CFM_FREE(terms[i].coeff); // free ith coefficient
      CFM_ADD_UP(ops, errorBound, errorBound, maxError); // accumulate rounding error
      // shift terms cheaply by swapping the ith and the last term
      terms[i].powers = terms[lastTermIndex].powers; // point ith powers to last
      terms[i].coeff = terms[lastTermIndex].coeff; // point ith coefficient to last
      lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
    }
  }
  if (MONOMIAL_DEGREE(terms[lastTermIndex].powers) > maxDeg) // reduce last term?
  {
    // QUESTION: cheaper to use a local Coeff coeffLast = terms[lastTermIndex].coeff ?

    CFM_ABS_UP(ops, maxError, terms[lastTermIndex].coeff); // compute reduction error
    CFM_FREE(terms[lastTermIndex].coeff); // free last coefficient
    CFM_ADD_UP(ops, errorBound, errorBound, maxError); // accumulate rounding error

    lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
  }
  CFM_FREE(maxError);
  CFM_ASSIGN(ops, p -> errorBound, errorBound); // account for rounding errors
  CFM_FREE(errorBound);

  lastTermIndex++; // set lastTermIndex to psize
  if (oldSize > lastTermIndex) // any terms reduced?
  {
    p -> psize = lastTermIndex; // update psize
  }
}

/*

void
ADD_COEFF_CODE(reduceDegreeUsingPureOps)(Ops_Pure * ops, Power maxDeg, Poly * p)
{
  Term * terms = p -> terms;
  Size oldSize = p -> psize;
  Size lastTermIndex = p -> psize - 1; // track last term for swapping
  for (int i = 0; i <= lastTermIndex; i++)
  {
    if (terms[i].degree > maxDeg) // reduce term?
    {
      // QUESTION: cheaper to use a local Coeff coeffi = terms[i].coeff ?

      Coeff maxError = CF_ABS_UP(ops, terms[i].coeff); // compute reduction error
      CF_FREE(terms[i].coeff); // free ith coefficient

      // account for reduction error
      Coeff oldErrorBound = p -> errorBound;
      p -> errorBound = CF_ADD_UP(ops, oldErrorBound, maxError);
      CF_FREE(oldErrorBound);
      CF_FREE(maxError);

      // shift terms cheaply by swapping the ith and the last term
      terms[i].degree = terms[lastTermIndex].degree; // overwrite ith degree with last
      terms[i].powers = terms[lastTermIndex].powers; // point ith power to last
      terms[i].coeff = terms[lastTermIndex].coeff; // point ith coefficient to last
      lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
    }
  }
  if (terms[lastTermIndex].degree > maxDeg) // reduce last term?
  {
    // QUESTION: cheaper to use a local Coeff coeffLast = terms[lastTermIndex].coeff ?

    Coeff maxError = CF_ABS_UP(ops, terms[lastTermIndex].coeff); // compute reduction error
    CF_FREE(terms[lastTermIndex].coeff); // free last coefficient

    // account for reduction error
    Coeff oldErrorBound = p -> errorBound;
    p -> errorBound = CF_ADD_UP(ops, oldErrorBound, maxError);
    CF_FREE(oldErrorBound);
    CF_FREE(maxError);

    lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
  }
  lastTermIndex++; // set lastTermIndex to psize
  if (oldSize > lastTermIndex) // any terms reduced?
  {
    p -> psize = lastTermIndex; // update psize
  }
}

typedef struct { Coeff coeff; int index; ComparisonOp compare; } CoeffFor234;

int compareFor234(CoeffFor234 * dp1, CoeffFor234 * dp2)
{
  return CF_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
}

tree234 *
ADD_COEFF_CODE(markTermsWithDegreeBelowAndLargestCoeffs)(ComparisonOp compare, Ops_Pure * ops,
    Term ** termsArray, Size termCount, Size maxSize, Power maxDegree)
{
  tree234 * markUs = newtree234(&compareFor234);
  Term * terms = *termsArray;
  Power degree = 0;
  int i = 0;
  // !!! add int counting the actual number of terms added to markUs
  // !!! make sure loops iterate the right number of times.
  while (i < maxSize)
  {
    if (terms[i].degree <= maxDegree) // anything to do?
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234));
      c -> coeff = CF_ABS_UP(ops, terms[i].coeff);
      c -> index = i;
      c -> compare = compare;
      add234(markUs, c);
    }
    i++;
  }
  while (i < termCount)
  {
    if (terms[i].degree <= maxDegree) // anything to do?
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234));
      c -> coeff = CF_ABS_UP(ops, terms[i].coeff);
      c -> index = i;
      c -> compare = compare;
      add234(markUs, c);
      CoeffFor234 * oldAbsCoeff = (CoeffFor234 *)delpos234(markUs, 0);
      CF_FREE(oldAbsCoeff -> coeff);
      free(oldAbsCoeff);
    }
    i++;
  }
  return markUs;
}

void ADD_COEFF_CODE(copy)(ComparisonOp compare, Ops_Pure * ops,
    Poly * res, Poly * src)
{
  Size srcSize = src -> psize;
  Term * srcTerms = src -> terms;

  Size resSize = res -> psize;
  Power maxDeg = res -> maxDeg;

  tree234 * termsToCopy =
    markTermsWithDegreeBelowAndLargestCoeffsGenCf(compare, ops,
      srcTerms, srcSize, resSize, maxDeg);
}

*/
