#include <string.h>
#include <stdlib.h>
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
  int cfcmp = CF_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
  return (cfcmp) ? cfcmp : (dp1 -> index - dp2 -> index);
  printf("compareFor234: compared\n");
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

  printf("copyEncl: srcDeg=%d resMaxDeg=%d srcPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, srcPsize, resMaxSize);

  if (curDeg <= resMaxDeg && srcPsize <= resMaxSize) // just copy src terms into res?
  {
    printf("copyEncl: no reduction needed\n");
    ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity,
        res, resTerms, resPsize,
        src, srcTerms, srcPsize);
    res -> psize = srcPsize;
    CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
  }
  else // some reduction will be needed
  {
    CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops)); // error accumulator
    CFM_ASSIGN(ops, errorBound, src -> errorBound);

    CoeffMutable absError = CFM_NEW(ops, CFM_SAMPLE(ops)); // reduction error
    int curLastTermIndex = srcPsize-1; // index of current last term in src
//    printf("copyEncl: curLastTermIndex=%d\n", curLastTermIndex);
    while (curDeg > resMaxDeg) // degree reduce i.e. truncate last term?
    {
      CFM_ABS_UP(ops, absError, srcTerms[curLastTermIndex].coeff); // compute truncation error
      CFM_ADD_UP(ops, errorBound, errorBound, absError); // accumulate error
      curLastTermIndex--; // forget truncated term
//      printf("copyEncl: degree reduced term\n");
      curDeg = (curLastTermIndex < 0) ? 0 : MONOMIAL_DEGREE(srcTerms[curLastTermIndex].powers);
//      printf("copyEncl: curDeg=%d curLastTermIndex=%d\n", curDeg, curLastTermIndex);
      printf("copyEncl: curDeg=%d resMaxDeg=%d curPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, curLastTermIndex+1, resMaxSize);
    }
    CFM_FREE(absError);
    Size curPsize = curLastTermIndex+1; // truncate reduced terms
//    printf("copyEncl: curDeg=%d resMaxDeg=%d curPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, curPsize, resMaxSize);

    int reductionsNeeded = curPsize-resMaxSize;
    printf("copyEncl: %d reductionsNeeded\n", reductionsNeeded);
    if (reductionsNeeded <= 0) // nothing to reduce i.e. copy remaining src terms into res?
    {
      ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity,
          res, resTerms, resPsize,
          src, srcTerms, curPsize);
    }
    else // curPsize > resMaxSize i.e. size reduction is needed
    {

      bool * reducedTerms = calloc(curPsize, sizeof(bool)); // set reduced terms to true, all initialised as false
      int termIndex = 0;

      tree234 * tree = newtree234(&compareFor234); // ordered storage
      printf("copyEncl: %d elements in tree\n", count234(tree));




      if (reductionsNeeded >= resMaxSize) // keep copied i.e. large terms in tree
      { // DONE

        int fullTreeSize = resMaxSize;

        printf("copyEncl: fullTreeSize=%d\n", fullTreeSize);
        while (termIndex < fullTreeSize) // fill tree with treeSize elements
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
          c -> index = termIndex;
          c -> compare = compare;
          add234(tree, c);
          printf("copyEncl: added element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));
          termIndex++;
        }
        printf("copyEncl: tree full\n");
        int * redIndexP; // index of reduced element
        CoeffFor234 * coeffP; // pointer to tree element
        while (termIndex < curPsize) // push an element and pop the smallest
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234)); // allocate an element
          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
          c -> index = termIndex;
          c -> compare = compare;

          add234(tree, c); // add the element
          printf("copyEncl: added element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));

          coeffP = findrelpos234(tree, NULL, NULL, REL234_GT, redIndexP); // pop the least element
          CFM_ADD_UP(ops, errorBound, errorBound, coeffP -> coeff); // compute reduction error
          CFM_FREE(coeffP -> coeff);

          reducedTerms[coeffP -> index] = true; // mark the original position of reduced term in srcTerms

          free(delpos234(tree, redIndexP)); // delete and free the element
          printf("copyEncl: deleted element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));
          termIndex++;
        }
        printf("copyEncl: reduction complete\n");
        for (int i = 0; i < fullTreeSize; i++) // free tree and contents of elements
        {
          coeffP = delpos234(tree, i);
          printf("copyEncl: deleted element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));
          CFM_FREE(coeffP -> coeff);
          free(coeffP); // REDUNDANT?
        }
        freetree234(tree);
        printf("copyEncl: freed tree\n");

        printf("copyEncl: curPsize=%d\n", curPsize);
        int srcIndex = 0;
        int resIndex = 0;
        printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
        while (srcIndex < curPsize)
        {
  //        printf("%d\n",(int)(reducedTerms[srcIndex] == false));
          if (reducedTerms[srcIndex] == false) // term not reduced i.e. copy term?
          {
            if (resPsize <= resIndex) // coefficient to be written in res not allocated?
            {
              resTerms[resIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
            }
            CFM_ASSIGN(ops, resTerms[resIndex].coeff, srcTerms[srcIndex].coeff); // copy the coefficient
            //    printf("copyTerms: copied coeff\n");
            memmove(resTerms[resIndex].powers, srcTerms[srcIndex].powers, SIZEOF_POWERS(srcArity)); // and powers
            //    printf("copyTerms: copied powers\n");
            printf("copyTerms: copied term\n");
            resIndex++;
          }
          srcIndex++;
          printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
        }
      }




      else // keep reduced i.e. small terms in the tree
      { // TODO
        int fullTreeSize = reductionsNeeded;

        printf("copyEncl: fullTreeSize=%d\n", fullTreeSize);
        while (termIndex < fullTreeSize) // fill tree with treeSize elements
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
          c -> index = termIndex;
          c -> compare = compare;
          add234(tree, c);
          printf("copyEncl: added element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));
          termIndex++;
        }
        printf("copyEncl: tree full\n");
        int * redIndexP; // index of reduced element
        CoeffFor234 * coeffP; // pointer to tree element
        while (termIndex < curPsize) // push an element and pop the smallest
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234)); // allocate an element
          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
          c -> index = termIndex;
          c -> compare = compare;

          add234(tree, c); // add the element
          printf("copyEncl: added element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));

//          coeffP = findrelpos234(tree, NULL, NULL, REL234_LT, redIndexP); // pop the greatest element
          findrelpos234(tree, NULL, NULL, REL234_LT, redIndexP); // pop the greatest element


          free(delpos234(tree, redIndexP)); // delete and free the element
          printf("copyEncl: deleted element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));
          termIndex++;
        }
        printf("copyEncl: reduction complete\n");
        for (int i = 0; i < fullTreeSize; i++) // free tree and contents of elements
        {
          coeffP = delpos234(tree, i);
          CFM_ADD_UP(ops, errorBound, errorBound, coeffP -> coeff); // compute reduction error
          CFM_FREE(coeffP -> coeff);
          reducedTerms[coeffP -> index] = true; // mark the original position of reduced term in srcTerms
          printf("copyEncl: deleted element\n");
          printf("copyEncl: %d elements in tree\n", count234(tree));
          CFM_FREE(coeffP -> coeff);
          free(coeffP); // REDUNDANT?
        }
        freetree234(tree);
        printf("copyEncl: freed tree\n");

        printf("copyEncl: curPsize=%d\n", curPsize);
        int srcIndex = 0;
        int resIndex = 0;
        printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
        while (srcIndex < curPsize)
        {
  //        printf("%d\n",(int)(reducedTerms[srcIndex] == false));
          if (reducedTerms[srcIndex] == false) // term not reduced i.e. copy term?
          {
            if (resPsize <= resIndex) // coefficient to be written in res not allocated?
            {
              resTerms[resIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
            }
            CFM_ASSIGN(ops, resTerms[resIndex].coeff, srcTerms[srcIndex].coeff); // copy the coefficient
            //    printf("copyTerms: copied coeff\n");
            memmove(resTerms[resIndex].powers, srcTerms[srcIndex].powers, SIZEOF_POWERS(srcArity)); // and powers
            //    printf("copyTerms: copied powers\n");
            printf("copyTerms: copied term\n");
            resIndex++;
          }
          srcIndex++;
          printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
        }
      }




      res -> psize = resMaxSize;
    }
    CFM_ASSIGN(ops, res -> errorBound, errorBound);
  }
  printf("copyEncl: res -> psize = %d\n", res -> psize);
  printf("copyEncl: exit\n");
}

///*
// * ASSUMES: deg <= maxDeg && size <= resMaxSize
// */
//ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops_Mutable * ops, Var arity,
//    Poly * res, Term * resTerms, Size resPsize,
//    Poly * src, Term * srcTerms, Size curPsize)
//{
//  printf("copyTerms: entry\n");
//  int i = 0; // save allocating and initialising one int by using two whiles
//  while (i < curPsize) // for each term in current version of src
//  {
//    if (resPsize <= i) // ith coefficient in res points to null?
//    {
//      resTerms[i].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
//    }
//    CFM_ASSIGN(ops, resTerms[i].coeff, srcTerms[i].coeff); // copy the coefficient
////    printf("copyTerms: copied coeff\n");
//    memmove(resTerms[i].powers, srcTerms[i].powers, SIZEOF_POWERS(arity)); // and powers
//    //    printf("copyTerms: copied powers\n");
//    i++;
//    printf("copyTerms: copied term\n");
//  }
//  while (i < resPsize) // any terms left unassigned in res?
//  {
//    CFM_FREE(resTerms[i].coeff); // free their coefficients
//    printf("copyTerms: freed coeff\n");
//    i++;
//  }
//  res -> psize = curPsize; // forget remaining terms in res
//  printf("copyTerms: exit\n");
//}
//
//typedef struct { CoeffMutable coeff; int index; ComparisonOp compare; } CoeffFor234;
//
//int compareFor234(CoeffFor234 * dp1, CoeffFor234 * dp2)
//{
//  return CF_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
//  printf("comparison\n");
//}
//
///*
// * ASSUMES: maxArity of src and res are the same
// * INVARIANT : does not change maxDeg and does not increase psize of res
// */
//
//void
//ADD_COEFF_CODE(copyEnclUsingMutableOps)(ComparisonOp compare, Ops_Mutable * ops,
//    Poly * res, Poly * src)
//{
//  printf("copyEncl: entry\n");
//
//  Var srcArity = src -> maxArity;
//  Size srcPsize = src -> psize;
//  Term * srcTerms = src -> terms;
//
//  Size resPsize = res -> psize;
//  Size resMaxSize = res -> maxSize;
//  Power resMaxDeg = res -> maxDeg;
//  Term * resTerms = res -> terms;
//
//  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);
//
//  Power curDeg = (srcPsize == 0) ? 0 : MONOMIAL_DEGREE(srcTerms[srcPsize-1].powers);
//
//  printf("copyEncl: srcDeg=%d resMaxDeg=%d srcPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, srcPsize, resMaxSize);
//
//  if (curDeg <= resMaxDeg && srcPsize <= resMaxSize) // just copy src terms into res?
//  {
//    printf("copyEncl: no reduction needed\n");
//    ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity,
//        res, resTerms, resPsize,
//        src, srcTerms, srcPsize);
//    CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
//  }
//  else // some reduction will be needed
//  {
//    CoeffMutable absError = CFM_NEW(ops, CFM_SAMPLE(ops)); // reduction error
//    CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops)); // accumulator
//    CFM_ASSIGN(ops, errorBound, src -> errorBound);
//    int curLastTermIndex = srcPsize-1; // index of current last term in src
//    printf("copyEncl: curLastTermIndex=%d\n", curLastTermIndex);
//    while (curDeg > resMaxDeg) // degree reduce i.e. truncate last term?
//    {
//      CFM_ABS_UP(ops, absError, srcTerms[curLastTermIndex].coeff); // compute truncation error
//      CFM_ADD_UP(ops, errorBound, errorBound, absError); // accumulate error
//      curLastTermIndex--; // forget truncated term
//      printf("copyEncl: degree reduced term\n");
//      curDeg = (curLastTermIndex < 0) ? 0 : MONOMIAL_DEGREE(srcTerms[curLastTermIndex].powers);
//      printf("copyEncl: curDeg=%d curLastTermIndex=%d\n", curDeg, curLastTermIndex);
//    }
//    Size curPsize = curLastTermIndex+1; // truncate reduced terms
//    printf("copyEncl: curDeg=%d resMaxDeg=%d curPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, curPsize, resMaxSize);
//    if (curPsize <= resMaxSize) // just copy remaining src terms into res?
//    {
//      ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity,
//          res, resTerms, resPsize,
//          src, srcTerms, curPsize);
//    }
//    else // curPsize-resMaxSize > 0 reductions are needed
//    {
//      CFM_FREE(absError); // not needed anymore
//
//      tree234 * tree = newtree234(&compareFor234);  // get a tree
//      int numReductions = curPsize-resMaxSize;
//      printf("copyEncl: numReductions=%d\n", numReductions);
//
//      if (resMaxSize < numReductions) // store the (few) large coefficients in the tree
//      {
//        CoeffFor234 * smallCoeffP;
//        bool * dontCopyMe = calloc(curPsize, sizeof(bool)); // mark the terms in src to be reduced as true
//        int termIndex = 0; // index of next srcTerm to be added to the tree
//        while (termIndex < resMaxSize) // fill tree with resMaxSize elements
//        {
//          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
//          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
//          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
//          c -> index = termIndex;
//          c -> compare = compare;
//          add234(tree, c);
//          printf("copyEncl: termIndex=%d\n", termIndex);
//          termIndex++;
//        }
//        printf("copyEncl: termIndex=%d\n", termIndex);
//        while (termIndex < curPsize) // add one and delete smallest from tree
//        {
//          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
//          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
//          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
//          c -> index = termIndex;
//          c -> compare = compare;
//          add234(tree, c);
//          smallCoeffP = (CoeffFor234 *)delpos234(tree, 0);
//          dontCopyMe[smallCoeffP -> index] = true; // marked term as reduced
//          CFM_ADD_UP(ops, errorBound, errorBound, smallCoeffP -> coeff); // accumulate reduction error
//          CFM_FREE(smallCoeffP -> coeff);
//          termIndex++;
//          printf("copyEncl: termIndex=%d\n", termIndex);
//        }
//        // free the tree!
//        printf("count234(tree)=%d\n", count234(tree));
//        int srcTermIndex = 0;
//        int resTermIndex = 0;
//        while (resTermIndex < resMaxSize)
//        {
//          if (dontCopyMe[srcTermIndex] == true) // copy the term?
//          {
//            if (resTermIndex >= resPsize) // resPsize[resTermIndex].coeff == NULL?
//            {
//              resTerms[resTermIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
//            }
//            CFM_ASSIGN(ops, resTerms[resTermIndex].coeff, srcTerms[srcTermIndex].coeff);
//            memmove(resTerms[resTermIndex].powers , srcTerms[srcTermIndex].powers, SIZEOF_POWERS(srcArity));
//            resTermIndex++;
//          }
//          srcTermIndex++;
//        }
//      }
//      else // store the (few) small coefficients in the tree
//      {
//        CoeffFor234 * largeCoeffP;
//        bool * copyMe = calloc(curPsize, sizeof(bool)); // mark the terms in src to be copied as true
//        int termIndex = 0; // index of next srcTerm to be added to the tree
//        while (termIndex < resMaxSize) // fill tree with lastTreeIndex+1 elements
//        {
//          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
//          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
//          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
//          c -> index = termIndex;
//          c -> compare = compare;
//          add234(tree, c);
//          printf("copyEncl: termIndex=%d\n", termIndex);
//          termIndex++;
//        }
//        printf("copyEncl: termIndex=%d\n", termIndex);
//        while (termIndex < curPsize) // add one and delete largest from tree
//        {
//          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
//          c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
//          CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
//          c -> index = termIndex;
//          c -> compare = compare;
//          add234(tree, c);
//          largeCoeffP = (CoeffFor234 *)delpos234(tree, numReductions);
//          copyMe[largeCoeffP -> index] = true; // marked term to be copied
//          CFM_ADD_UP(ops, errorBound, errorBound, largeCoeffP -> coeff); // accumulate reduction error
//          CFM_FREE(largeCoeffP -> coeff);
//          termIndex++;
//          printf("copyEncl: termIndex=%d\n", termIndex);
//        }
//        printf("count234(tree)=%d\n", count234(tree));
//        int srcTermIndex = 0;
//        int resTermIndex = 0;
//        while (resTermIndex < resMaxSize)
//        {
//          if (copyMe[srcTermIndex] == true) // copy the term?
//          {
//            if (resTermIndex >= resPsize) // resPsize[resTermIndex].coeff == NULL?
//            {
//              resTerms[resTermIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
//            }
//            CFM_ASSIGN(ops, resTerms[resTermIndex].coeff, srcTerms[srcTermIndex].coeff);
//            memmove(resTerms[resTermIndex].powers , srcTerms[srcTermIndex].powers, SIZEOF_POWERS(srcArity));
//            resTermIndex++;
//          }
//          srcTermIndex++;
//        }
//      }
//    }
//    CFM_ASSIGN(ops, res -> errorBound, errorBound);
//  }
//  printf("copyEncl: exit\n");
//}

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
