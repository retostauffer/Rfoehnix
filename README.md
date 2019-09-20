
# foehnix <a href='https://retostauffer.github.io/Rfoehnix/'><img src='https://github.com/retostauffer/Rfoehnix/raw/master/foehnix-logo.png' align="right" height="139" /></a>

[![Build Status](https://api.travis-ci.org/retostauffer/Rfoehnix.svg?branch=master)](https://api.travis-ci.org/retostauffer/Rfoehnix.svg?branch=master)
[![codecov](https://codecov.io/gh/retostauffer/Rfoehnix/branch/master/graph/badge.svg)](https://codecov.io/gh/retostauffer/Rfoehnix)
[![Repository Status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/badges/latest/wip.svg)
[![Python](https://img.shields.io/badge/Python-githubio-yellowgreen)](https://matthiasdusch.github.io/foehnix-python/_build/html/)


# A Toolbox for Automated Foehn Classification based on Mixture Models

_foehnix_ package provides a toolbox for automated probabilistic foehn
wind classification based on two-component mixture models (**foehn**
m**ix**ture models).
_foehnix_ models are a special case of the general flexible mixture model
class ([Fraley 2002](#fraley2000), [Leisch 2004](#leisch2004), [Gr&uuml;n
2007](#gruen2007), [Gr&uuml;n 2008](#gruen2008)), an unsupervised statistical
model to identify unobserveable clusters or components in data sets.

The application of mixture models for an automated classification of foehn winds
has first been proposed by
[Plavcan et al. (2014)](#plavcan2014).
The "Community Foehn Classification Experiment"
shows that the method performs similar compared to another semi-automatic classification,
foehn experts, students, and weather enthusiasts (see [Mayr 2019](mayr2018)).

Aim of this software package:

* provide **easy-to-use functions** for classification
* create **probabilistic foehn classification**
* **easy scalability** (can be applied to large data sets)
* **reproducibility** of the results
* create results which are **comparable to other locations**


### Important Links

* [_R foehnix_ documentation](http://retostauffer.github.io/Rfoehnix) available on github.
* [Python version of foehnix](https://github.com/matthiasdusch/foehnix-python), also available on github.



# First Steps

## Installation

The package is not yet published via the _Comprehensive R Archive Network_
([CRAN](https://cran.r-project.org)) but will be made available as soon as finished.
Currently the package has to be downloaded/installed via [github](https://github.com/retostauffer/Rfoehnix) which can be done in different ways.
**Note** that the _foehnix_ package comes with routines written in _C_. Thus, on Windows,
the [Rtools](https://cran.r-project.org/bin/windows/Rtools/) have to be installed to be
able to compile the _C_ code!

### Via the _R_ package _remotes_

The _R_ package [remotes](https://cran.r-project.org/package=remotes) allows one
to install packages from [github](https://github.com) via the _R_ command line interface
using the function `install_github`. By default, [remotes](https://cran.r-project.org/package=remotes)
installes the latest version of the `master` branch.
However, feel free to install one of our earlier releases
([show release candidates](https://github.com/retostauffer/Rfoehnix/releases)).


``` r
# Load library 'remotes'. If not yet installed, call
# install.packages("remotes") first.
library("remotes")

# Install package (automatically resolves required dependencies)
install_github("retostauffer/Rfoehnix")

# Or install a specific release candiate (here v0.1-2)
install_github("retostauffer/Rfoehnix@v0.1-2")
asdf
```


### By cloning the repository

Or do it the good old way via git clone (Note: dependencies have to
be installed manually):

``` bash
# Change directory
cd <somewhere/on/your/local/disc>

# Clone repository
git clone https://github.com/retostauffer/Rfoehnix.git foehnix
```

Afterwards use your preferred way to install local packages. E.g.,
using [devtools](https://cran.r-project.org/package=devtools) or 
[remotes](https://cran.r-project.org/package=remotes):

```
# Use remotes
remotes::install_local("foehnix")

# Using devtools
devtools::install("foehnix")
```

### For Command Shell Fans

Fans of the good old console can of course use the good old way.

``` r
# Change directory
cd <somewhere/on/your/local/disc>

# Clone repository
git clone https://github.com/retostauffer/Rfoehnix.git foehnix

# Feel free to use on of our release candidates which
# might miss some features, but might also be more stable.
# A list of release candidates can be found on:
# - https://github.com/retostauffer/Rfoehnix/releases
(cd foehnix && git checkout tags/<tagname> && cd ..)

# Install directly
R CMD INSTALL foehnix

# OR
R CMD build foehnix
R CMD INSTALL foehnix_<version>.tar.gz
```

## Import observation data

The _foehnix_ package depends on the _R_ package
[_zoo_](https://cran.r-project.org/package=zoo) (Z's Ordered Observations), a
package/object to handle (un-)regular time series data.
Import data as _zoo_ is relatively straight forward for those being used to
_R_. For all others, we provide a small "how-to" which can be found here:

* [Import data for _foehnix_](articles/import_data.html)

More information is provided in the
"[Reading Data into zoo](https://cran.r-project.org/web/packages/zoo/vignettes/zoo-read.pdf)"
vignette of the _R_ package [_zoo_](https://cran.r-project.org/package=zoo).


## Create classification

Once the observation data have been imported, one can start doing the
classification. The _foehnix_ package comes with two demo data sets,
one for Southern California (USA) and one for Tyrol (A).
The documentation provides a walk-through on how to start using
_foehnix_:

* Demo for [Ellbögen (Tyrol, A)](articles/ellboegen.html)
* Demo for [Viejas (California, USA)](articles/viejas.html)




<!--
# Current State

The current version of _foehnix_ is currently in beta state.
The documentation and vignettes will be updated soon to provide more
information, however, the main features all work well and the usage should
not change (much).

If there are any questions please do not hesitate to contact me (via github).

All best,
-->

_[Reto](https://github.com/retostauffer/Rfoehnix), Matthias, Georg, and Fabien._

### References

<p id="mayr2018">
Mayr GJ, Plavcan D, Laurence A, Elvidge A, Grisogono B, Horvath K, Jackson P,
Neururer A, Seibert P, Steenburgh JW, Stiperski I, Sturman A, Ve&#269;enaj
&#381;, Vergeiner J, Vosper S, Z&auml;ngl G (2018).  The Community Foehn
Classification Experiment.
<i>Bulletin of the American Meteorological Society</i>, <b>99</b>(11), 2229&mdash;2235,
<a href="https://doi.org/10.1175/BAMS-D-17-0200.1" target="_blank">10.1175/BAMS-D-17-0200.1</a>
</p>

<p id="plavcan2014">
Plavcan D, Mayr GJ, Zeileis A (2014).
Automatic and Probabilistic Foehn Diagnosis with a Statistical Mixture Model.
<i>Journal of Applied Meteorology and Climatology</i>, <b>53</b>(3), 652&mdash;659,
<a href="https://dx.doi.org/10.1175/JAMC-D-13-0267.1" target="_blank">10.1175/JAMC-D-13-0267.1</a>
</p>

<p id="hastie2009">
Hastie T, Tibshirani R, Friedman J (2009).
Fitting Logistic Regression Models. In <i>The Elements of Statistical Learning</i>
(Chapter 4.4.1), 2<i>nd</i> edition, ISBN 978-0387848570.
<a href="https://web.stanford.edu/~hastie/ElemStatLearn/" target="_blank">PDF download</a>
</p>

<p id="gruen2008">
Gr&uuml;n B, Friedrich L (2008).
FlexMix Version 2: Finite Mixtures with Concomitant Variables and Varying and Constant Parameters.
<i>Journal of Statistical Software, Articles</i>, <b>28</b>(4), 1&mdash;35,
doi:<a href="https://dx.doi.org/10.18637/jss.v028.i04" target="_blank">10.18637/jss.v028.i04</a>
</p>

<p id="gruen2007">
Gr&uuml;n B, Leisch F (2007).
Fitting Finite Mixtures of Generalized Linear Regressions in _R_.
<i>Computational Statistics & Data Analysis</i>, <b>51</b>(11),
doi:<a href="https://dx.doi.org/10.1016/j.csda.2006.08.014" target="_blank">10.1016/j.csda.2006.08.014</a>
</p>

<p id="leisch2004">
Friedrich L (2004).
FlexMix: A General Framework for Finite Mixture Models and Latent Class Regression in <i>R</i>.
<i>Journal of Statistical Software, Articles</i>, <b>11</b>(8), 1&mdash;18,
doi:<a href="https://dx.doi.org/10.18637/jss.v011.i08" target="_blank">10.18637/jss.v011.i08</a>
</p>

<p id="fraley2000">
Fraley C, Raftery AE (2000).
Model-Based Clustering, Discriminant Analysis, and Density Estimation.
<i>Journal of the American Statistical Association</i>, <b>97</b>(458), 611&mdash;631,
doi:<a href="https://dx.doi.org/10.1198/016214502760047131" target="_blank">10.1198/016214502760047131</a>
</p>

<p id="mccullagh1999">
McCullagh P, Nelder JA (1999).
Likelihood functions for binary data. In <i>Generalized Linear Models</i> (Chapter 4.4),
2<i>nd</i> edition, ISBN 0-412-31760-5.
</p>


