
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
#> '/opt/microsoft/ropen/3.4.1/lib64/R/bin/R' --no-site-file --no-environ  \
#>   --no-save --no-restore --quiet CMD INSTALL  \
#>   '/tmp/RtmpUXmdqg/devtools5ff85447ed3c/seonghobae-kaefa-5452ca4'  \
#>   --library='/home/development/kaefa/packrat/lib/x86_64-pc-linux-gnu/3.4.1'  \
#>   --install-tests
#> 
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library('kaefa')
mod1 <- kaefa::aefa(mirt::Science)
#>      item        Zh     S_X2 df.S_X2    p.S_X2    PV_Q1 df.PV_Q1   p.PV_Q1
#> 1 Comfort 0.8257384 4.437619       6 0.6176746 11.54161 10.76667 0.3795823
#> 2    Work 2.0027930 8.863065       9 0.4500095 20.14887 17.00000 0.2666858
#> 3  Future 5.3042086 7.536471       8 0.4800050 13.22452 10.43333 0.2396807
#> 4 Benefit 1.9453938 9.971430      11 0.5329589 19.25101 17.43333 0.3409037
mod1
#> $estModelTrials
#> $estModelTrials[[1]]
#> 
#> Call:
#> mirt::mirt(data = data, model = i, itemtype = j, SE = T, method = "MHRM", 
#>     calcNull = T, key = key, GenRandomPars = GenRandomPars, accelerate = accelerate, 
#>     technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, 
#>         symmetric = symmetric))
#> 
#> Full-information item factor analysis with 1 factor(s).
#> Converged within 0.001 tolerance after 113 MHRM iterations.
#> mirt version: 1.25.6 
#> M-step optimizer: NR1 
#> 
#> Information matrix estimated with method: MHRM
#> Condition number of information matrix = 112.5461
#> Second-order test: model is a possible local maximum
#> 
#> Log-likelihood = -1608.696, SE = 0.022
#> Estimated parameters: 16 
#> AIC = 3249.392; AICc = 3250.843
#> BIC = 3312.933; SABIC = 3262.165
#> G2 (239) = 213.14, p = 0.8844
#> RMSEA = 0, CFI = 1, TLI = 1.196
#> 
#> $itemFitTrials
#> $itemFitTrials[[1]]
#>      item        Zh     S_X2 df.S_X2    p.S_X2    PV_Q1 df.PV_Q1   p.PV_Q1
#> 1 Comfort 0.8257384 4.437619       6 0.6176746 11.54161 10.76667 0.3795823
#> 2    Work 2.0027930 8.863065       9 0.4500095 20.14887 17.00000 0.2666858
#> 3  Future 5.3042086 7.536471       8 0.4800050 13.22452 10.43333 0.2396807
#> 4 Benefit 1.9453938 9.971430      11 0.5329589 19.25101 17.43333 0.3409037
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
