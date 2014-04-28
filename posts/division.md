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
\begin{equation}\label{eq:using-y}
\frac{1}{f(x_1,x_2,\ldots)} 
= 
y\left(\frac{2(f(x_1,x_2,\ldots)-b)}{c-b}-1\right)\frac{2}{c-b}
=
y\left(\frac{2}{c-b}f(x_1,x_2,\ldots)-d\right)\frac{2}{c-b}
\end{equation}
$$
where $d=1+\frac{2b}{c-b}$.

### The tau method

We need to approximate $y(x) = \frac{1}{x+d}$.
Following the tau method as described, for example, in 
[Mason & Handscob (2002)](http://books.google.co.uk/books/about/Chebyshev_Polynomials.html?id=8FHf0P3to0UC&redir_esc=y) 
on page 62, 
our approximation of $y(x)$ is a polynomial $p(x)$ with the following property:
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

Before discussing how to compute $p(x)$ and $\tau$ satisfying $\eqref{eq:tau}$, let us
analyse the difference between $p(x)$ and $y(x)$ and the resulting error of the
approximate reciprocal obtained using $p(x)$ in place of $y(x)$. 
$$
\begin{equation}\label{eq:error}
|p(x) - y(x)|
=
\left|T_{k+1}(x) \frac{\tau}{x+d}\right| 
\leq 
\frac{\tau}{x+d}
\leq
\frac{\tau}{d-1}
=
\frac{\tau(c-b)}{2b}
\end{equation}
$$

Not surprisingly, for $b$ close to $0$, the error could be very large.
Nevertheless, if $\tau$ converges to $0$ with increasing $k$, the
the error can be minimised arbitrarily.

#### Computing p(x)

Both sides of equation $\eqref{eq:tau}$ are polynomials.  Thus the coefficients of the polynomials must be equal.
We focus on coefficients of these polynomials in the 
[Chebyshev basis](http://en.wikipedia.org/wiki/Chebyshev_polynomials).
Let us denote the (unknown) coefficients of $p(x)$ as follows:
$$
p(x) = c_0 + c_1T_1(x) + \ldots + c_k T_k(x)
$$


By elementary properties of Chebyshev polynomials it holds:
$$
T_i(x)\cdot(x+d) = d\cdot T_i(x) + \frac{T_{|i-1|}(x) + T_{i+1}(x)}{2}
$$

Now by grouping the terms that contain Chebyshev polynomials $T_i$ on both sides equation $\eqref{eq:tau}$,
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
Moreover, this system is very easy to solve in linear time, proceeding from
top to bottom in the above list and determining the value of $\tau$ from the last equation.
