
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mstATA

<!-- badges: start -->

<!-- badges: end -->

The goal of mstATA is to …

## Installation

You can install the development version of **mstATA** from
[GitHub](https://github.com/) using `pak`:

``` r
# install.packages("pak")
pak::pak("Hongchen030/mstATA")
#> ! Using bundled GitHub PAT. Please add your own PAT using `gitcreds::gitcreds_set()`.
#> ✔ Updated metadata database: 7.78 MB in 10 files.
#> ℹ Updating metadata database✔ Updating metadata database ... done
#>  
#> → Will install 1 package.
#> → Will download 1 package with unknown size.
#> + mstATA   0.0.0.9000 👷🏿‍♂️🔧 ⬇ (GitHub: ab1075d)
#> ℹ Getting 1 pkg with unknown size
#> ✔ Got mstATA 0.0.0.9000 (source) (791.58 kB)
#> ℹ Packaging mstATA 0.0.0.9000
#> ✔ Packaged mstATA 0.0.0.9000 (1.2s)
#> ℹ Building mstATA 0.0.0.9000
#> ✔ Built mstATA 0.0.0.9000 (3.2s)
#> ✔ Installed mstATA 0.0.0.9000 (github::Hongchen030/mstATA@ab1075d) (22ms)
#> ✔ 1 pkg + 33 deps: kept 15, added 1, dld 1 (NA B) [18.7s]
```

Alternatively, you can use `remotes`:

``` r
# install.packages("remotes")
remotes::install_github("Hongchen030/mstATA")
#> Skipping install of 'mstATA' from a github remote, the SHA1 (ab1075dc) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

`mstATA` supports multiple mixed-integer programming solvers, including:

- Gurobi
- HiGHS
- GLPK
- Symphony
- lpSolve

Some solvers (e.g., Gurobi) require separate installation and licenses.

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
