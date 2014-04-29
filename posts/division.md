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
y(x) = \frac{1}{x+d}
$$
where $x\in[-1,1]$ and $d>1$ will be specified later.

We can get an approximation of the original reciprocal 
using an approximation of $y$ as follows:
$$
\begin{equation}\label{eq:using-y}
\frac{1}{f(x_1,x_2,\ldots)} 
= 
y\left(\frac{2}{c-b}f(x_1,x_2,\ldots)-d\right)\frac{2}{c-b}
\end{equation}
$$
where we used the substitution:
$$
\begin{equation}\label{eq:x}
x = \frac{2}{c-b}f(x_1,x_2,\ldots)-d
\end{equation}
$$
The scaling and shifting of $f$ in $\eqref{eq:x}$ should ensure that $x$ falls within $[-1,1]$.
To make it so, we set $d=1+\frac{2b}{c-b}$.

### The tau method

In order to approximate $y(x)$,
we follow the tau method as described, for example, in 
[Mason & Handscob (2002)](http://books.google.co.uk/books/about/Chebyshev_Polynomials.html?id=8FHf0P3to0UC&redir_esc=y) 
on page 62.
The approximation of $y(x)$ is a polynomial $p(x)$ with the following property:
$$
\begin{equation}\label{eq:tau}
p(x)\cdot (x+d) = 1 + \tau \cdot T_{k+1}(x)
\end{equation}
$$
where $T_i$ is the Chebyshev polynomial of order $i$.
We will soon show that there is a unique polynomial $p(x)$ and unique $\tau$
that satisfy the above equality.
It is clear that this polynomial has to have order at most $k$.

#### Estimating the error

Before discussing how to compute $p(x)$ and $\tau$ that satisfy $\eqref{eq:tau}$, let us
analyse the difference between $p(x)$ and $y(x)$ and the resulting error of the
approximate reciprocal obtained using $p(x)$ in place of $y(x)$. 
$$
\begin{equation}\label{eq:error}
|p(x) - y(x)|
=
\left|T_{k+1}(x) \frac{\tau}{x+d}\right| 
\leq 
\left|\frac{\tau}{x+d}\right| 
=
\frac{|\tau|}{x+d}
\leq
\frac{|\tau|}{d-1}
=
\frac{|\tau|(c-b)}{2b}
\end{equation}
$$

Not surprisingly, for $b$ close to $0$, the error could be very large.
Nevertheless, if $\tau$ converges to $0$ with increasing $k$,
the error can be minimised arbitrarily.

#### Computing p(x)

Both sides of equation $\eqref{eq:tau}$ are polynomials.  Thus the coefficients of the polynomials must be equal.
We focus on coefficients of these polynomials in the 
[Chebyshev basis](http://en.wikipedia.org/wiki/Chebyshev_polynomials).
Let us denote the (unknown) coefficients of $p(x)$ as follows:
$$
\begin{equation}\label{eq:px}
p(x) = c_0 + c_1T_1(x) + \ldots + c_k T_k(x)
\end{equation}
$$


By elementary properties of Chebyshev polynomials it holds:
$$
T_i(x)\cdot(x+d) = d\cdot T_i(x) + \frac{T_{|i-1|}(x) + T_{i+1}(x)}{2}
$$
which leads to:
$$
\begin{array}{rcl}
p(x)\cdot(x+d) 
& = & c_0 \cdot\left(  d\cdot T_0(x) + T_1(x) \right)
\\
& + & c_1 \cdot\left( T_0(x)/2 + d\cdot T_1(x) + T_2(x)/2 \right)
\\
& \ldots &
\\ 
& + & c_{k-1} \cdot\left( T_{k-2}(x)/2 + d\cdot T_{k-1}(x) + T_k(x)/2 \right)
\\ 
& + & c_k \cdot\left( T_{k-1}(x)/2 + d\cdot T_k(x) + T_{k+1}(x)/2 \right)
\\ 
\end{array}
$$

Now by grouping the terms that contain Chebyshev polynomials $T_i$ on both sides of equation $\eqref{eq:tau}$,
we get:
$$
\begin{array}{rrcl}
T_{k+1}(x): & \tau & = & c_k / 2
\\
T_{k}(x): & 0 & = & d\cdot c_k + c_{k-1}/2
\\
T_{k-1}(x): & 0 & = & c_{k}/2 + d\cdot c_{k-1} + c_{k-2}/2
\\
& & \ldots
\\[1ex]
T_{2}(x): & 0 & = & c_{3}/2 + d\cdot c_{2} + c_{1}/2
\\
T_{1}(x): & 0 & = & c_{2}/2 + d\cdot c_{1} + c_{0}
\\
T_{0}(x): & 1 & = & c_{1}/2 + d\cdot c_{0}
\\
\end{array}
$$

We have $k+2$ linear equations with $k+2$ unknowns $c_0,\ldots,c_k,\tau$.
Moreover, by a simple transformation we get formulas for the individual coefficients:

$$
\begin{equation}\label{eq:coeffs-tau}
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
\end{equation}
$$
This system is solved in linear time as follows: Proceeding by substitutions from
top to bottom in the above list we obtain a formula for each coefficient, featuring only $\tau$,
and, finally, we determine the value of $\tau$ from the last equation.
