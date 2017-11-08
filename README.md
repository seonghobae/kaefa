
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- run rmarkdown::render('README.Rmd', output_file = 'README.md', encoding = 'utf8') -->
kaefa
=====

The goal of kaefa is to improving research capability to identify unexplained factor structure with complexly cross-classified multilevel structured data in R environment with automated exploratory factor analysis (aefa) framework

Installation
------------

You can install kaefa from github with:

``` r
# install.packages("devtools")
devtools::install_github("seonghobae/kaefa")
#> Downloading GitHub repo seonghobae/kaefa@master
#> from URL https://api.github.com/repos/seonghobae/kaefa/zipball/master
#> Installing kaefa
#> '/usr/lib/R/bin/R' --no-site-file --no-environ --no-save --no-restore  \
#>   --quiet CMD INSTALL  \
#>   '/tmp/Rtmp4s2vBj/devtools49e077f0096f/seonghobae-kaefa-4ee128e'  \
#>   --library='/home/seongho_iopsy/kaefa/packrat/lib/x86_64-pc-linux-gnu/3.4.1'  \
#>   --install-tests
#> 
#> Reloading installed kaefa
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library('kaefa')
mod1 <- kaefa::aefa(mirt::Science)
#>      item        Zh      S_X2 df.S_X2    p.S_X2    PV_Q1 df.PV_Q1
#> 1 Comfort 0.8306699  4.767522       6 0.5739572 12.45069 10.90000
#> 2    Work 1.9598670 10.500243      10 0.3977535 21.09088 17.06667
#> 3  Future 5.4155546  7.599225       8 0.4735642 12.75249 10.03333
#> 4 Benefit 1.8726442 10.034175      11 0.5273158 21.37382 17.26667
#>     p.PV_Q1
#> 1 0.3229984
#> 2 0.2256005
#> 3 0.2401209
#> 4 0.2230962
mod1
#> $estModelTrials
#> $estModelTrials[[1]]
#> 
#> Call:
#> mirt::mirt(data = data, model = model, itemtype = j, SE = T, 
#>     method = "MHRM", calcNull = T, key = key, GenRandomPars = GenRandomPars, 
#>     accelerate = accelerate, technical = list(NCYCLES = NCYCLES, 
#>         BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))
#> 
#> Full-information item factor analysis with 1 factor(s).
#> Converged within 0.001 tolerance after 132 MHRM iterations.
#> mirt version: 1.25.2 
#> M-step optimizer: NR1 
#> 
#> Information matrix estimated with method: MHRM
#> Condition number of information matrix = 51.41857
#> Second-order test: model is a possible local maximum
#> 
#> Log-likelihood = -1608.54, SE = 0.022
#> Estimated parameters: 16 
#> AIC = 3249.081; AICc = 3250.531
#> BIC = 3312.621; SABIC = 3261.853
#> G2 (239) = 212.78, p = 0.8879
#> RMSEA = 0, CFI = 1, TLI = 1.199
#> 
#> $itemFitTrials
#> $itemFitTrials[[1]]
#>      item        Zh      S_X2 df.S_X2    p.S_X2    PV_Q1 df.PV_Q1
#> 1 Comfort 0.8306699  4.767522       6 0.5739572 12.45069 10.90000
#> 2    Work 1.9598670 10.500243      10 0.3977535 21.09088 17.06667
#> 3  Future 5.4155546  7.599225       8 0.4735642 12.75249 10.03333
#> 4 Benefit 1.8726442 10.034175      11 0.5273158 21.37382 17.26667
#>     p.PV_Q1
#> 1 0.3229984
#> 2 0.2256005
#> 3 0.2401209
#> 4 0.2230962
```

software quality information
----------------------------

### ubuntu and mac environment

[![Travis-CI Build Status](https://travis-ci.org/seonghobae/kaefa.svg?branch=master)](https://travis-ci.org/seonghobae/kaefa)

### windows environment

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/seonghobae/kaefa?branch=master&svg=true)](https://ci.appveyor.com/project/seonghobae/kaefa)

<!-- ### code quality -->
<!-- [![Coverage Status](https://img.shields.io/codecov/c/github/seonghobae/kaefa/master.svg?maxAge=3600)](https://codecov.io/github/seonghobae/kaefa?branch=master) -->
[Contributor Code of Conduct](CONDUCT.md)
