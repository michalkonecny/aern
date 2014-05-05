---
title: logisticmap
author: Michal Konečný
date: 2014-05-02
mathjax: on
---

For example, consider
the [logistic map](http://en.wikipedia.org/wiki/Logistic_map), defined by the simple recurrence formula:
$$
x_{n+1} = rx_n(1-x_n)
$$
For almost all $r\in[3.567,4]$ this system is chaotic.

A [direct encoding in AERN](https://github.com/michalkonecny/aern/blob/master/aern-mpfr/demos/LogisticMap.hs) 
with arbitrary precision support
in the style of [iRRAM](http://irram.uni-trier.de/) computes tight interval enclosures for
$x_n$.  For example, with $r=3.75$ and $x_0=0.5$, it computes:

<!--
$n$    time          computed enclosure of $x_n$                     
-----  ------ ----   --------------------------------
10     0.008s        $0.77910939751004...\pm 10^{-160}$
100    0.02s         $0.38545043842252...\pm 10^{-160}$
1000   0.5s          $0.69714851192442...\pm 10^{-160}$
10000  72s           $0.67381614749956...\pm 10^{-160}$
100000 72s           $0.76860294939511...\pm 10^{-160}$
-->

<table class="table table-striped table-bordered">
<tr>
<th>$n$ (Number of iterations)</th><th>Approx. computation time</th><th>Computed enclosure of $x_n$</th>
</tr>
<tr>
<td>$10$</td><td>0.01 s</td><td>$0.77910939751004...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$100$</td><td>0.02 s</td><td>$0.38545043842252...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$1000$</td><td>0.5 s</td><td>$0.69714851192442...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$10,000$</td><td>70 s</td><td>$0.67381614749956...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$100,000$</td><td>3 h</td><td>$0.76860294939511...\pm 10^{-160}$</td>
</tr>
</table>

Note that iterating the logistic map is a highly unstable computation.  Evaluating 
it directly using Double precision gives completely meaningless results 
after not many iterations.  For example, for 100 iterations using Double
precision one gets $0.66...$, while the correct number is close to $0.38...$. 

To compute 100,000 iterations with a good
precision, the program used floating-point numbers with mantissa size 
over 200,000 bits.  Over 2 hours of the 3-hour computation time was spent
trying and failing to perform the computation with various lower precisions.

<!-- 
TODO: Demonstrate that the same formula can be used to evaluate on real numbers 
as well as on real functions to capture dependency.

Show how x_1..x_5 depend on x_0 \in [0,1].

Plot how x_n depends on n \in [0,10] when r = 4 using the explicit formula.  
-->

