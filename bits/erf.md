---
title: erf
author: Michal Konečný
date: 2014-06-07
mathjax: on
---

AERN can approximate real numbers by real intervals and real continuous functions by 
*function intervals*.  For example, [this code](https://github.com/michalkonecny/aern/blob/master/aern-poly-plot-gtk/demos/erf.hs) 
computes approximations of the error function
$$
\begin{equation}
    \mathrm{erf}(x) =\frac{2}{\sqrt{\pi}}\int_0^x e^{-t^2}\,dt
\end{equation}
$$
over the domain $x\in[0,2]$.  Each computed approximation is an interval $[f(x),g(x)]$ where
$f$ and $g$ are polynomials such that
$f(x) \leq \mathrm{erf}(x) \leq g(x)$.
We say that the interval $[f(x),g(x)]$ is an enclosure of the function $\mathrm{erf}(x)$.

[The program](https://github.com/michalkonecny/aern/blob/master/aern-poly-plot-gtk/demos/erf.hs) 
produces the following enclosures:

<a href="/img/erf-enclosures.png">
<img 
    src="/img/erf-enclosures.png" height="300"
    alt="Enclosures of the error function over the domain [0,2]"
>
</a>

Some of the plotted enclosures are rather inaccurate and others are so tight that their
thickness is not visible at this zoom level.  The quality of the computed enclosure
depends on the effort used during the computation.  The effort can be set via so-called
effort parameters.

For approximating $\mathrm{erf}(x)$ in AERN there are the following effort parameters:

  * The precision of polynomial coefficients and constants such as $\pi$
  * An upper bound on the polynomial degree
  * The Taylor degree for approximating $e^x$
  
In our example, all enclosures are computed using the machine Double 
for polynomial coefficients and interval endpoints, and using the Taylor degree 5.  
The upper bound on polynomial degree ranges from 5 to 25.
