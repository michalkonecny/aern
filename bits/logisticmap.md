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
100    0.02s         $3.8545043842252...\pm 10^{-160}$
1000   0.5s          $6.9714851192442...\pm 10^{-160}$
10000  1min          $6.7381614749956...\pm 10^{-160}$
-->

<table class="table table-striped table-bordered">
<tr>
<th>$n$ (Number of iterations)</th><th>Approx. computation time</th><th>Computed enclosure of $x_n$</th>
</tr>
<tr>
<td>$100$</td><td>0.02 s</td><td>$3.8545043842252...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$1000$</td><td>0.5 s</td><td>$6.9714851192442...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$10000$</td><td>72 s</td><td>$6.7381614749956...\pm 10^{-160}$</td>
</tr>
</table>
