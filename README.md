
<!-- README.md is generated from README.Rmd. Please edit that file -->
kaefa
=====

The goal of kaefa is to improving research capability to identify unexplained factor structure with complexly cross-classified multilevel structured data in R environment with automatory exploratory factor analysis (aefa) framework

Installation
------------

You can install kaefa from github with:

``` r
# install.packages("devtools")
devtools::install_github("seonghobae/kaefa")
#> Skipping install of 'kaefa' from a github remote, the SHA1 (d0a6f41c) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library('kaefa')
mod1 <- kaefa::aefa(mirt::Science)
#>      item        Zh      S_X2 df.S_X2    p.S_X2    PV_Q1 df.PV_Q1   p.PV_Q1
#> 1 Comfort 0.7866104  4.184038       5 0.5232348 12.76940     10.7 0.2864025
#> 2    Work 1.9401849 10.677309      10 0.3832049 19.74770     17.0 0.2873372
#> 3  Future 5.3370136  7.669541       8 0.4663975 13.76204     10.3 0.2019186
#> 4 Benefit 1.9005341  9.988865      11 0.5313891 19.51184     17.3 0.3178982
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
#> Converged within 0.001 tolerance after 57 MHRM iterations.
#> mirt version: 1.25.2 
#> M-step optimizer: NR1 
#> 
#> Information matrix estimated with method: MHRM
#> Condition number of information matrix = 108.615
#> Second-order test: model is a possible local maximum
#> 
#> Log-likelihood = -1609.199, SE = 0.022
#> Estimated parameters: 16 
#> AIC = 3250.399; AICc = 3251.849
#> BIC = 3313.939; SABIC = 3263.172
#> G2 (239) = 214.14, p = 0.8746
#> RMSEA = 0, CFI = 1, TLI = 1.188
#> 
#> $itemFitTrials
#> $itemFitTrials[[1]]
#>      item        Zh      S_X2 df.S_X2    p.S_X2    PV_Q1 df.PV_Q1   p.PV_Q1
#> 1 Comfort 0.7866104  4.184038       5 0.5232348 12.76940     10.7 0.2864025
#> 2    Work 1.9401849 10.677309      10 0.3832049 19.74770     17.0 0.2873372
#> 3  Future 5.3370136  7.669541       8 0.4663975 13.76204     10.3 0.2019186
#> 4 Benefit 1.9005341  9.988865      11 0.5313891 19.51184     17.3 0.3178982
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
