[![pipeline status](https://gitlab.com/fvafrCU/maSAE/badges/master/pipeline.svg)](https://gitlab.com/fvafrCU/maSAE/commits/master)    
[![coverage report](https://gitlab.com/fvafrCU/maSAE/badges/master/coverage.svg)](https://gitlab.com/fvafrCU/maSAE/commits/master)
<!-- 
    [![Build Status](https://travis-ci.org/fvafrCU/maSAE.svg?branch=master)](https://travis-ci.org/fvafrCU/maSAE)
    [![Coverage Status](https://codecov.io/github/fvafrCU/maSAE/coverage.svg?branch=master)](https://codecov.io/github/fvafrCU/maSAE?branch=master)
-->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/maSAE)](https://cran.r-project.org/package=maSAE)
[![RStudio_downloads_monthly](https://cranlogs.r-pkg.org/badges/maSAE)](https://cran.r-project.org/package=maSAE)
[![RStudio_downloads_total](https://cranlogs.r-pkg.org/badges/grand-total/maSAE)](https://cran.r-project.org/package=maSAE)

<!-- README.md is generated from README.Rmd. Please edit that file -->



# maSAE
## Introduction
Please read the
[vignette](https://CRAN.R-project.org/package=maSAE/vignettes/maSAE.pdf).
<!-- 

-->

Or, after installation, the help page:

```r
help("maSAE-package", package = "maSAE")
```

```
#> Mandallaz' Model-Assisted Small Area Estimators
#> 
#> Description:
#> 
#>      An S4 implementation of the unbiased extension of the
#>      model-assisted' synthetic-regression estimator proposed by
#>      Mandallaz (2013), Mandallaz et al. (2013) and Mandallaz (2014).
#>      It yields smaller variances than the standard bias correction, the
#>      generalised regression estimator.
#> 
#> Details:
#> 
#>      This package provides Mandallaz' extended synthetic-regression
#>      estimator for two- and three-phase sampling designs with or
#>      without clustering.
#>      See vignette("maSAE", package = "maSAE") and demo("maSAE", package
#>      = "maSAE") for introductions, '"class?maSAE::saeObj"' and
#>      '"?maSAE::predict"' for help on the main feature.
#> 
#> Note:
#> 
#>      Model-assisted estimators use models to improve the efficiency
#>      (i.e. reduce prediction error compared to design-based estimators)
#>      but need not assume them to be correct as in the model-based
#>      approach, which is advantageous in official statistics.
#> 
#> References:
#> 
#>      Mandallaz, D. 2013 Design-based properties of some small-area
#>      estimators in forest inventory with two-phase sampling. Canadian
#>      Journal of Forest Research *43*(5), pp. 441-449. doi:
#>      \Sexpr[results=rd,stage=build]{tools:::Rd_expr_doi("10.1139/cjfr-2012-0381")}.
#> 
#>      Mandallaz, and Breschan, J.  and Hill, A. 2013 New regression
#>      estimators in forest inventories with two-phase sampling and
#>      partially exhaustive information: a design-based Monte Carlo
#>      approach with applications to small-area estimation. Canadian
#>      Journal of Forest Research *43*(11), pp. 1023-1031. doi:
#>      \Sexpr[results=rd,stage=build]{tools:::Rd_expr_doi("10.1139/cjfr-2013-0181")}.
#> 
#>      Mandallaz, D. 2014 A three-phase sampling extension of the
#>      generalized regression estimator with partially exhaustive
#>      information. Canadian Journal of Forest Research *44*(4), pp.
#>      383-388. doi:
#>      \Sexpr[results=rd,stage=build]{tools:::Rd_expr_doi("10.1139/cjfr-2013-0449")}.
#> 
#> See Also:
#> 
#>      There are a couple packages for model-*based* small area
#>      estimation, see 'sae', 'rsae', hbsae and 'JoSAE'. In 2016, Andreas
#>      Hill published 'forestinventory', another implementation of
#>      Mandallaz' model-assisted small area estimators (see
#>      'vignette("forestinventory_and_maASE", package = "maSAE")' for a
#>      comparison).
#> 
#> Examples:
#> 
#>      ## Not run:
#>      
#>      vignette("maSAE", package = "maSAE")
#>      ## End(Not run)
#>      
#>      ## Not run:
#>      
#>      demo("design", package = "maSAE")
#>      ## End(Not run)
#>      
#>      ## Not run:
#>      
#>      demo("maSAE", package = "maSAE")
#>      ## End(Not run)
#> 
```

## Installation

You can install maSAE from gitlab via:


```r
if (! require("remotes")) install.packages("remotes")
remotes::install_gitlab("fvafrCU/maSAE")
```


