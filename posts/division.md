---
title: Approximate Polynomial Division
author: Michal Konečný
date: 2014-04-27
mathjax: on
---

### The goal

The goal is to find a polynomial approximation of the reciprocal 
$$
\frac{1}{f(x_1,x_2,\ldots)}
$$ 
where each variable $x_i$ ranges over some compact real interval $D_i$
and the range of $f$ over these domains is within the interval $[b,c]$
where $b > 0$.

### Reduction to a unary problem

To simplify the task, we split it into two sub-tasks.
The first sub-task is to approximate the unary reciprocal
$$
y(x) = \frac{1}{x+1+\frac{2b}{c-b}}
$$
where $x\in[-1,1]$,
and then compute an approximation of the original reciprocal using an
approximation of $y$ as follows:
$$
\frac{1}{f(x_1,x_2,\ldots)} = y\left(\frac{2(f(x_1,x_2,\ldots)-b)}{c-b}-1\right)\frac{2}{c-b}
$$

### The tau method

Let $d=1+\frac{2b}{c-b}$.  We need to approximate $y(x) = \frac{1}{x+d}$.
Following the tau method as described, for example, in Mason et al (2006) on page 62, 
our approximation of $y(x)$ is the unique polynomial $p(x)$ with the following property:
$$
p(x)\cdot (x+d) = 1 + \tau \cdot T_{k+1}(x)
$$
where $T_i$ is the Chebyshev polynomial of order $i$.  The smaller is $\tau>0$, the smaller is
the error of the approximation $p(x)$.

Both sides of the equality are polynomials.  Thus the coefficients of the polynomials must be equal.
We focus on coefficients of these polynomials in the Chebyshev basis.
Let us denote the (unknown) coefficients of $p(x)$ as follows:
$$
p(x) = c_0 + c_1T_1(x) + \ldots + c_k T_k(x)
$$

Recall that by the elementary properties of the Chebyshev polynomials we have:
$$
T_i(x)\cdot(x+d) = d\cdot T_i(x) + \frac{T_{|i-1|}(x) + T_{i+1}(x)}{2}
$$

Now by grouping the terms that contain Chebyshev polynomials $T_i$ on both sides of the equation,
we get:
$$
\begin{array}{rcl}
c_k & = & 2\tau
\\
c_{k-1} & = & -2dc_k
\\
c_{k-2} & = & -2dc_{k-1}-c_k
\\
& \dots &
\\
c_1 & = & -2dc_2 - c_3
\\
c_0 & = & -dc_1 - c_2/2
\\
1 & = & dc_0 + c_1/2 
\end{array}
$$
Now we have $k+2$ linear equations with $k+2$ unknowns $c_0,\ldots,c_k,\tau$.
Moreover, this system is very easy to solve in linear time.
