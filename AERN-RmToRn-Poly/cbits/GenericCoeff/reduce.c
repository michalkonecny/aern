#include <string.h>
#include <tree234.h>

#include <GenericCoeff/coeff.h>
#include <GenericCoeff/poly.h>

/*
 * ASSUMES: deg <= maxDeg && size <= resMaxSize
 */
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops_Mutable * ops, Var arity,
    Poly * res, Term * resTerms, Size resPsize,
    Poly * src, Term * terms, Size psize)
{
  printf("copyTerms: entry\n");
  int i = 0; // save allocating and initialising one int by using two whiles
  while (i < psize) // for each term in src
  {
    if (resPsize <= i) // ith coefficient in res points to null?
    {
      resTerms[i].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
    }
    CFM_ASSIGN(ops, resTerms[i].coeff, terms[i].coeff); // copy the coefficient
    printf("copyTerms: copied coeff\n");
    memmove(resTerms[i].powers, terms[i].powers, SIZEOF_POWERS(arity)); // and powers
    printf("copyTerms: copied powers\n");
    i++;
  }
  while (i < resPsize) // any terms left unassigned in res?
  {
    printf("copyTerms: free coeff\n");
    CFM_FREE(resTerms[i].coeff); // free their coefficients
    i++;
  }
  res -> psize = psize; // forget remaining terms in res
  printf("copyTerms: exit\n");
}

typedef struct { CoeffMutable coeff; int index; ComparisonOp compare; } CoeffFor234;

int compareFor234(CoeffFor234 * dp1, CoeffFor234 * dp2)
{
printf("compare\n");
  return CF_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
}

/*
 * ASSUMES: maxArity of src and res are the same
 * INVARIANT : does not change maxDeg and does not increase psize of res
 */

void
ADD_COEFF_CODE(copyEnclUsingMutableOps)(ComparisonOp compare, Ops_Mutable * ops,
    Poly * res, Poly * src)
{
  printf("copyEncl: entry\n");
  Var arity = src -> maxArity;
  Size psize = src -> psize;
  Term * terms = src -> terms;
  Power deg = (psize == 0) ? 0 : MONOMIAL_DEGREE(terms[psize-1].powers);

  Size resPsize = res -> psize;
  Size resMaxSize = res -> maxSize;
  Power resMaxDeg = res -> maxDeg;
  Term * resTerms = res -> terms;

  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);
  printf("copyEncl: assigned constTerm\n");

  printf("copyEncl: deg=%d maxDeg=%d size=%d resSize=%d maxSize=%d\n",
                    deg, resMaxDeg, psize, resPsize, resMaxSize);
  if (deg <= resMaxDeg && psize <= resMaxSize) // just copy src terms into res?
  {
    printf("copyEncl: just copy terms\n");
    ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, arity,
        res, resTerms, resPsize,
        src, terms, psize);
    printf("copyEncl: assign errorBound\n");
    CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
  }
  else // some reduction will be needed
  {
    CoeffMutable absError = CFM_NEW(ops, CFM_SAMPLE(ops)); // reduction error
    CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops)); // accumulator
    CFM_ASSIGN(ops, errorBound, src -> errorBound);
    while (deg > resMaxDeg) // degree reduce i.e. truncate last term?
    {
      printf("degree reducing");
      psize--; // now size == index of truncated term
      CFM_ABS_UP(ops, absError, terms[psize].coeff); // compute truncation error
      CFM_ADD_UP(ops, errorBound, errorBound, absError); // accumulate error
      deg = (psize == 0) ? 0 : MONOMIAL_DEGREE(terms[psize].powers); // update degree
    }
    if (psize <= resMaxSize) // just copy remaining src terms into res?
    {
      ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, arity,
          res, resTerms, resPsize,
          src, terms, psize);
      CFM_ASSIGN(ops, res -> errorBound, errorBound);
    }
    else // size-resMaxSize reductions are needed
    {
      tree234 * tree = newtree234(&compareFor234);  // get a tree
      int last = psize-resMaxSize-1; // index of last element in the full tree
      while (psize > resMaxSize) // fill the tree with last+1 elements
      {
        psize--; // now size == index of last term in degree reduced src
        CoeffFor234 * c = malloc(sizeof(CoeffFor234));
        c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
        printf("0 size=%d resSize=%d last=%d\n", psize, resPsize, last);
        CFM_ABS_UP(ops, c -> coeff, terms[psize].coeff);
        printf("1\n");
        c -> index = psize;
        c -> compare = compare;
        add234(tree, c);
        printf("3\n");
      }
      int nextResTermIndex = 0;
      while (psize > 0) // add one and delete last from tree, using it to copy to res
      {
        psize--; //
        CoeffFor234 * c = malloc(sizeof(CoeffFor234));
        c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
        CFM_ABS_UP(ops, c -> coeff, terms[psize].coeff);
        printf("4\n");
        c -> index = psize;
        c -> compare = compare;
        add234(tree, c);
        printf("5\n");
        CoeffFor234 * oldAbsCoeff = (CoeffFor234 *)delpos234(tree, last);

        printf("6 size=%d last=%d nextResTermIndex=%d (oldAbsCoeff -> index)=%d\n",
          psize, last, nextResTermIndex, oldAbsCoeff -> index);
        // copy terms[oldAbsCoeff -> index] into resTerms[nextResTermIndex]
        CFM_ASSIGN(ops, resTerms[nextResTermIndex].coeff, terms[oldAbsCoeff -> index].coeff);
        printf("7\n");
        memmove(resTerms[nextResTermIndex].powers, terms[oldAbsCoeff -> index].powers, SIZEOF_POWERS(arity));
        printf("8\n");
        CFM_FREE(oldAbsCoeff -> coeff); // free the deleted tree element's coefficient
        printf("9\n");
        free(oldAbsCoeff); // and the deleted element
        printf("10\n");
        nextResTermIndex++;
      }
      while (last >= 0) // accumulate the reduction errors while emptying the tree
      {
        CoeffFor234 * oldAbsCoeff = (CoeffFor234 *)delpos234(tree, last);
        CFM_ADD_UP(ops, errorBound, errorBound, oldAbsCoeff -> coeff);
        CFM_FREE(oldAbsCoeff -> coeff);
        last--;
      }
      CFM_ASSIGN(ops, res -> errorBound, errorBound);
    }
  }
  printf("exit from copyEnclUsingMutableOps reached");
}

//tree234 *
//ADD_COEFF_CODE(markTermsWithDegreeBelowAndLargestCoeffs)(ComparisonOp compare, Ops_Pure * ops,
//    Term ** termsArray, Size termCount, Size maxSize, Power maxDegree)
//{
//  tree234 * markUs = newtree234(&compareFor234);
//  Term * terms = *termsArray;
//  Power degree = 0;
//  int i = 0;
//  // !!! add int counting the actual number of terms added to markUs
//  // !!! make sure loops iterate the right number of times.
//  while (i < maxSize)
//  {
//    if (terms[i].degree <= maxDegree) // anything to do?
//    {
//      CoeffFor234 * c = malloc(sizeof(CoeffFor234));
//      c -> coeff = CF_ABS_UP(ops, terms[i].coeff);
//      c -> index = i;
//      c -> compare = compare;
//      add234(markUs, c);
//    }
//    i++;
//  }
//  while (i < termCount)
//  {
//    if (terms[i].degree <= maxDegree) // anything to do?
//    {
//      CoeffFor234 * c = malloc(sizeof(CoeffFor234));
//      c -> coeff = CF_ABS_UP(ops, terms[i].coeff);
//      c -> index = i;
//      c -> compare = compare;
//      add234(markUs, c);
//      CoeffFor234 * oldAbsCoeff = (CoeffFor234 *)delpos234(markUs, 0);
//      CF_FREE(oldAbsCoeff -> coeff);
//      free(oldAbsCoeff);
//    }
//    i++;
//  }
//  return markUs;
//}

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
