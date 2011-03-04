#include <string.h>
#include <tree234.h>

#include <GenericCoeff/coeff.h>
#include <GenericCoeff/poly.h>



/*
 * ASSUMES: deg <= maxDeg && size <= resMaxSize
 */
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops_Mutable * ops, Var arity,
    Poly * res, Term * resTerms, Size resPsize,
    Poly * src, Term * srcTerms, Size curPsize)
{
  printf("copyTerms: entry\n");
  int i = 0; // save allocating and initialising one int by using two whiles
  while (i < curPsize) // for each term in current version of src
  {
    if (resPsize <= i) // ith coefficient in res points to null?
    {
      resTerms[i].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
    }
    CFM_ASSIGN(ops, resTerms[i].coeff, srcTerms[i].coeff); // copy the coefficient
//    printf("copyTerms: copied coeff\n");
    memmove(resTerms[i].powers, srcTerms[i].powers, SIZEOF_POWERS(arity)); // and powers
    //    printf("copyTerms: copied powers\n");
    i++;
    printf("copyTerms: copied term\n");
  }
  while (i < resPsize) // any terms left unassigned in res?
  {
    CFM_FREE(resTerms[i].coeff); // free their coefficients
    printf("copyTerms: freed coeff\n");
    i++;
  }
  res -> psize = curPsize; // forget remaining terms in res
  printf("copyTerms: exit\n");
}

typedef struct { CoeffMutable coeff; int index; ComparisonOp compare; } CoeffFor234;

int compareFor234(CoeffFor234 * dp1, CoeffFor234 * dp2)
{
  return CF_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
  printf("comparison\n");
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

  Var srcArity = src -> maxArity;
  Size srcPsize = src -> psize;
  Term * srcTerms = src -> terms;

  Size resPsize = res -> psize;
  Size resMaxSize = res -> maxSize;
  Power resMaxDeg = res -> maxDeg;
  Term * resTerms = res -> terms;

  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);

  Power curDeg = (srcPsize == 0) ? 0 : MONOMIAL_DEGREE(srcTerms[srcPsize-1].powers);

  printf("copyEncl: srcPsize=%d resMaxSize=%d srcDeg=%d resMaxDeg=%d\n", srcPsize, resMaxSize, curDeg, resMaxDeg);

  if (curDeg <= resMaxDeg && srcPsize <= resMaxSize) // just copy src terms into res?
  {
    printf("copyEncl: no reduction needed\n");
    ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity,
        res, resTerms, resPsize,
        src, srcTerms, srcPsize);
    CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
  }
  else // some reduction will be needed
  {
//    printf("copyEncl: some reduction needed\n");
    CoeffMutable absError = CFM_NEW(ops, CFM_SAMPLE(ops)); // reduction error
    CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops)); // accumulator
    CFM_ASSIGN(ops, errorBound, src -> errorBound);
    int currentPsize = srcPsize; // current
    int curLastTermIndex = currentPsize-1; // index of current last term in src
    printf("copyEncl: curLastTermIndex=%d\n", curLastTermIndex);
    while (curDeg > resMaxDeg) // degree reduce i.e. truncate last term?
    {
      CFM_ABS_UP(ops, absError, srcTerms[curLastTermIndex].coeff); // compute truncation error
      CFM_ADD_UP(ops, errorBound, errorBound, absError); // accumulate error
      curLastTermIndex--; // forget truncated term
      printf("copyEncl: degree reduced term\n");
      curDeg = (curLastTermIndex < 0) ? 0 : MONOMIAL_DEGREE(srcTerms[curLastTermIndex].powers);
      printf("copyEncl: curDeg=%d curLastTermIndex=%d\n", curDeg, curLastTermIndex);
    }
    Size curPsize = curLastTermIndex+1; // truncate reduced terms
    printf("copyEncl: curPsize=%d resMaxSize=%d curDeg=%d resMaxDeg=%d\n", curPsize, resMaxSize, curDeg, resMaxDeg);
    if (curPsize < resMaxSize) // just copy remaining src terms into res?
    {
      ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity,
          res, resTerms, resPsize,
          src, srcTerms, srcPsize);
      CFM_ASSIGN(ops, res -> errorBound, errorBound);
    }
    else // size-resMaxSize reductions are needed
    {
      tree234 * tree = newtree234(&compareFor234);  // get a tree
      int last = srcPsize-resMaxSize-1; // index of last element in the full tree
      while (srcPsize > resMaxSize) // fill the tree with last+1 elements
      {
        srcPsize--; // now size == index of last term in degree reduced src
        CoeffFor234 * c = malloc(sizeof(CoeffFor234));
        c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
        CFM_ABS_UP(ops, c -> coeff, srcTerms[srcPsize].coeff);
        c -> index = srcPsize;
        c -> compare = compare;
        add234(tree, c);
      }
      int nextResTermIndex = 0;
      while (srcPsize > 0) // add one and delete last from tree, using it to copy to res
      {
        srcPsize--; //
        CoeffFor234 * c = malloc(sizeof(CoeffFor234));
        c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
        CFM_ABS_UP(ops, c -> coeff, srcTerms[srcPsize].coeff);
        c -> index = srcPsize;
        c -> compare = compare;
        add234(tree, c);
        CoeffFor234 * coeffToCopy = (CoeffFor234 *)delpos234(tree, last);

        // copy terms[oldAbsCoeff -> index] into resTerms[nextResTermIndex]
        if (resPsize <= nextResTermIndex) // unallocated coeff?
        {
          resTerms[nextResTermIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
        }
        CFM_ASSIGN(ops, resTerms[nextResTermIndex].coeff, srcTerms[coeffToCopy -> index].coeff);
        memmove(resTerms[nextResTermIndex].powers, srcTerms[coeffToCopy -> index].powers, SIZEOF_POWERS(srcArity));
        CFM_FREE(coeffToCopy -> coeff); // free the deleted tree element's coefficient
        free(coeffToCopy); // and the deleted element
        nextResTermIndex++;
      }
      printf("tree size = %d\n", count234(tree));
      int j = 0;
      CoeffFor234 * oldAbsCoeff;
      while ((oldAbsCoeff = (CoeffFor234 *)delpos234(tree, j)) != NULL) // accumulate errors
      {
        CFM_ADD_UP(ops, errorBound, errorBound, oldAbsCoeff -> coeff);
        CFM_FREE(oldAbsCoeff -> coeff);
        j++;
      }
      CFM_ASSIGN(ops, res -> errorBound, errorBound);
    }
  }
  printf("copyEncl: exit\n");
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
