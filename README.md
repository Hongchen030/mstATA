<!-- README.md is generated from README.Rmd. Please edit that file -->

# mstATA

<!-- badges: start -->

[![R-CMD-check](https://github.com/Hongchen030/mstATA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Hongchen030/mstATA/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of mstATA is to …

## Installation

You can install the development version of **mstATA** from
[GitHub](https://github.com/) using `pak`:

``` r
# install.packages("pak")
pak::pak("Hongchen030/mstATA")
```

Alternatively, you can use `remotes`:

``` r
# install.packages("remotes")
remotes::install_github("Hongchen030/mstATA")
```

`mstATA` supports multiple mixed-integer programming solvers, including:

Gurobi – requires separate installation and a valid license.
Installation instructions are available here:
<https://CRAN.R-project.org/package=prioritizr>. See the prioritizr
package vignette “gurobi_installation_guide” for installation
instructions. Gurobi requires a valid license. However, free academic
licenses are available for qualified students and researchers through
the Gurobi Academic Program.

HiGHS – available on CRAN via install.packages(“highs”)

GLPK (via Rglpk) – available on CRAN via install.package(“Rglpk”)

SYMPHONY (via Rsymphony) – available on CRAN via
install.package(“Rsymphony”)

lpSolve (via lpSolveAPI) – available on CRAN via
install.package(“lpSolveAPI”)

## Vignettes

The package includes several vignettes:

- **Getting Started with mstATA** – complete end-to-end workflow.
- **MST Assembly Examples: Bottom-Up, Top-Down, and Hybrid Strategies**
  – bottom-up, top-down, and extended-hybrid strategies.
- **Stimulus-Based Assessment in mstATA: Conditional Item Selection** –
  conditional item selection with stimulus constraints.
- **Formulation for Multiple Objectives** – multi-objective optimization
  strategies and evaluation.
- **Detecting and Resolving Infeasibility** - how to deal with
  infeasibility.

To browse available vignettes:

``` r
browseVignettes("mstATA")
#> No vignettes found by browseVignettes("mstATA")
```

## Citation

If you use `mstATA` in research, please cite:

Chen, H. (2026). mstATA: An R Package for IRT-Based Multistage Test
Assembly.
