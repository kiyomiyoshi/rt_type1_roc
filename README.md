
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

## RT-based type-1 ROC construction

Our approach for constructing type-1 ROC based on RT is explained using
a three-level RT bin example. First, RTs are divided into three
equal-sized bins, with stimulus class (absent/present) and response
(yes/no) collapsed. Figure a below illustrates this process, where t1
and t2 represent the cutoff thresholds defining the RT tertiles, and b1,
b2, and b3 correspond to the fastest, second-fastest, and slowest RT
bins, respectively. Trials for each stimulus class are thus
characterized by an assigned response (yes/no) and an RT bin (three
levels), classified in six response categories. Figure b shows the
response frequency of these categories, arranged from left to right to
indicate decreasing support for “yes” judgment (e.g., “no” responses in
the fastest RT bin represent the weakest indication of “yes” judgment).
For each stimulus class, cumulative response proportions are calculated
sequentially from left to right, which corresponds to hit and FA rates
in type-1 ROC space (Figure c).

<figure>
<img src="main/figures/figure_2.jpg" alt="Figure_2" />
<figcaption aria-hidden="true">Figure_2</figcaption>
</figure>

## Files

The `perception` folder includes data and code for the main manuscript,
where `analysis_perception.R` implements all the analyses reported.  
The `memory` folder includes data and code for the supplementary
material, where `analysis_memory.R` implements the relevant analyses.
