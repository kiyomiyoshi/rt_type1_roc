
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RT Type-1 ROC

This is the repository for the manuscript “Correcting for Unequal
Variance in Signal Detection Models Using Response Time,” available at
<https://doi.org/10.31234/osf.io/cr9k6_v1>.

## Unequal-variance signal detection theory analysis

The code `uvsdt.R` defines the function `fit_uvsdt_mle()` for model
fitting.  
`nr_s1` and `nr_s2` are response frequency vectors for S1
(target-absent) and S2 (target present) trials, ordered from fastest-RT
(or highest-confidence) Yes to fastest-RT (or highest-confidence) No
responses.  
`add_constant = TRUE` adds a small value to the response frequency
vectors for estimation stability (default value is TRUE).

``` r
nr_s1 <- c(20,  36, 16, 40, 82, 137)
nr_s2 <- c(197, 42, 12, 17, 30, 32)

source("uvsdt.R")
f1 <- fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
f1
#>         mu    sigma       da     cri.X1    cri.X2    cri.X3    cri.X4   cri.X5
#> 1 1.950602 1.684445 1.408211 -0.2220643 0.4270594 0.7694734 0.9530994 1.544301
#>        logL
#> 1 -933.9624
```

## Files

`perception` folder includes data and code for the main manuscript,
where `analysis_perception.R` implements all the analyses reported.  
`memory` folder includes data and code for the supplementary material,
where `analysis_memory.R` implements the relevant analyses.
