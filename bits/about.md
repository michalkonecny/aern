---
title: test
author: Michal Konečný
date: 2014-04-27
mathjax: on
---

## What can AERN do?


### Compute (tight) interval bounds and function enclosures

The [logistic map](http://en.wikipedia.org/wiki/Logistic_map) is defined by the simple recurrence formula
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
<th>$n$</th><th>computation time</th><th>computed enclosure of $x_n$</th>
</tr>
<tr>
<td>$100$</td><td>0.02s</td><td>$3.8545043842252...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$1000$</td><td>0.5s</td><td>$6.9714851192442...\pm 10^{-160}$</td>
</tr>
<tr>
<td>$10000$</td><td>72s</td><td>$6.7381614749956...\pm 10^{-160}$</td>
</tr>
</table>

----

### Enclose solutions of differential equations

_[under construction]_

### Enclose solutions of hybrid systems

_[under construction]_

### Prove or disprove numerical theorems

_[under construction]_
 
---
 
## Origin

  * AERN has been under development since 2005, mainly at [Aston University](http://www.aston.ac.uk/)
  * AERN was initially inspired by the C++ library [iRRAM](http://irram.uni-trier.de/)
  * The project has been sponsored by EPSRC and Altran Praxis



