---
title: AERN | Installing from source code
author: Michal Konečný
date: 2014-07-10
mathjax: on
---

The specific parts of these instructions are applicable only to a Debian/Ubuntu Linux.

Nevertheless, these instructions should be adaptable to any modern desktop operating system.

----

  1. Ensure you have a [Haskell Platform](https://www.haskell.org/platform/) installation or similar (tested with [GHC](http://www.haskell.org/ghc/) 7.6.3)
  
```sh
> sudo apt-get install haskell-platform
> sudo cabal update
> cabal update
```

----

  2. (Optional) Install [MPFR](http://www.mpfr.org/) if you require support for arbitrary precision arithmetic.
  
  **Note:** *If you require arbitrary-precision arithmetic using MPFR, you need a modified GHC as described on [this page](https://hackage.haskell.org/package/hmpfr).*

```sh
> sudo apt-get install libmpfr-dev
```

----

  3. (Optional) Install [Haskell support](http://projects.haskell.org/gtk2hs/) for 
  [GTK+](http://www.gtk.org/), [Cairo](http://cairographics.org/examples/) 
  and [Glade](https://glade.gnome.org/) if you require support for plotting.
  
```sh
> sudo apt-get install libghc-cairo-dev libghc-glade-dev  
```
----

  4. Clone the [GitHub master branch](https://github.com/michalkonecny/aern) where you want to build AERN

```sh    
> git clone https://github.com/michalkonecny/aern.git
```
    
----

  5. Run an installation script 
    
  
```sh
> cd aern
> . install-all.sh 
```

If you do not want support for MPFR and/or GTK, use the appropriately named `install-*.sh` script instead. 

This script invokes [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install) 
for each of the AERN packages.  See [the script code](https://github.com/michalkonecny/aern/blob/master/install-all.sh) 
to see in what order the packages are installed.
        
The first time the script is executed, it is likely to install a large number of dependencies, which can take a long time. 

On subsequent executions, the script is likely to give a warning about dangerous reinstalls.  This warning can be safely ignored in this case.
  
----

  6. You can now execute a demo or a test suite to check the installation has been successful, eg:

Test arithmetic of intervals with Double endpoints:

```sh
> ~/.cabal/bin/testAERN-Real-Double
```

Before running examples that produce plots, make sure your current folder contains
the file `FnView.glade`.  The base folder of the `aern` repository contains this file already.

Plot some function enclosures:

```sh
> ~/.cabal/bin/plot minmax 10
```

Compute and plot an enclosure of an ODE IVP with uncertain initial value:

```sh
> ~/.cabal/bin/simple-ode ivpSpringMass-uv 10 "PlotGraph[True,False](0,10,-1.5,1.5)" GUI ShrinkWrap 30 200 -10 -10
```

Compute and plot an enclosure of a hybrid system with Zeno behaviour:

```sh
> ~/.cabal/bin/simple-events bouncingBallFloorDropEnergy 4.4 "PlotGraph[True,False,True,False,True](0,5,-0.2,5)" GUI locate evtree 10 10 3 50 0 0 
```

You can also execute various demos using ghci.
