
#include <coeff.h>
#include <poly.h>

void
ADD_COEFF_CODE(reduceDegreeInPlaceUsingPureOps)(Ops_Pure * ops, Poly * p, Power maxDeg)
{
  Term * terms = p -> terms;
  Size pMaxDeg = p -> maxDeg;
  if (pMaxDeg > maxDeg) // anything to do?
  {
    Size lastTermIndex = p -> psize - 1; // track last term in case reduction occurs
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
        terms[i].powers = terms[lastTermIndex].powers; // overwrite ith power with last
        terms[i].coeff = terms[lastTermIndex].coeff; // overwrite ith coefficient with last
        lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
      }
    }
    p -> psize = lastTermIndex; // update psize
    p -> maxDeg = maxDeg; // update maxDeg
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

/*
 * ASSUMES: res -> maxArity == src -> maxArity
 *
 * WARNING: does not check ASSUMPTION
 */

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
