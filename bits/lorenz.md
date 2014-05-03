---
title: lorenz
author: Michal Konečný
date: 2014-05-03
mathjax: on
---

AERN contains an [ODE IVP](http://en.wikipedia.org/wiki/Initial_value_problem) solver.
The main executable of the solver is in the file [simple-ode.hs](https://github.com/michalkonecny/aern/blob/master/aern-ivp/demos/simple-ode.hs).

First consider the [simple harmonic motion](http://en.wikipedia.org/wiki/Simple_harmonic_motion) initial value problem
$$
x'' = -x\qquad x(0) \in 1\pm 0.125\quad x'(0) \in 0\pm 0.125
$$
which is formally specified in the module 
[Numeric.AERN.IVP.Examples.ODE.Simple](https://github.com/michalkonecny/aern/blob/master/aern-ivp/src/Numeric/AERN/IVP/Examples/ODE/Simple.hs)
under the name "ivpSpringMass_uv".

To enclose the solutions of this system, we can run <code>simple-ode</code>
as follows:

    simple-ode ivpSpringMass-uv "PlotGraph[True,False](0,10,-1.5,1.5)" GUI False 30 200 -10 -10 10

producing the following enclosure:

<a href="/img/simple-ode-SpringMass-uv-PlotGraph-0to10x-1.5to1.5-False-30-200--10--10-10.png">
<img 
    src="/img/simple-ode-SpringMass-uv-PlotGraph-0to10x-1.5to1.5-False-30-200--10--10-10.png" height="300"
    alt="Enclosure of spring mass IVP with uncertain initial condition"
>
</a>

As an example system with chaotic behaviour,
consider the classical instance of the [Lorenz initial value problem](http://en.wikipedia.org/wiki/Lorenz_system), 
given by the following ordinary differential equation (ODE) and initial value:
$$
\begin{array}{rcl}
x' & = & 10(y-x)\\ 
y' & = & x(28-z)-y\\ 
z' & = & xy - 8z/3
\end{array}
\qquad
\begin{array}{rcl}
x(0) & \in & 15\pm\delta\\ 
y(0) & \in & 15\pm\delta\\ 
z(0) & \in & 36\pm\delta
\end{array}
\qquad
\delta = 0.01
$$
which is formally specified in the module 
[Numeric.AERN.IVP.Examples.ODE.Simple](https://github.com/michalkonecny/aern/blob/master/aern-ivp/src/Numeric/AERN/IVP/Examples/ODE/Simple.hs)
under the name "ivpLorenz_uv".

To enclose the solutions of this system, we can run <code>simple-ode</code>
as follows:

    simple-ode ivpLorenz-uv "PlotParam[True,True](-20,20,-30,30)" GUI False 12 200 8 8 4

producing an enclosure whose projection to the $x$-$y$ plane looks as follows:

<a href="/img/simple-ode-ivpLorenz-uv-PlotParam-20to20x-30to30-False-12-200-6-6-4.png">
<img 
    src="/img/simple-ode-ivpLorenz-uv-PlotParam-20to20x-30to30-False-12-200-6-6-4.png" height="300"
    alt="Enclosure of spring mass IVP with uncertain initial condition"
>
</a>

AERN solve-ode is an experimental solver using an unusual method.  It is based on a
direct implementation of the interval Picard operator introduced by 
[Edalat & Pattinson (2007)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.122.1105).
This method of computing solutions is simple, almost correct by construction.  Thus one can have a high level of confidence that the solver produces enclosures
that contain the exact solution(s).  Established enclosure ODE solvers such as 
[VNODE](http://www.cas.mcmaster.ca/~nedialk/Software/VNODE/VNODE.shtml) and 
[COSY](http://www.bt.pa.msu.edu/index_cosy.htm) 
use more
complex methods and are more optimised.  The AERN solve-ode solver does not compete with
the established solvers in terms of speed and ability to enclose solutions over long time intervals
but we expect it will be easier to validate and verify than the established solvers.
