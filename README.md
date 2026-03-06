
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
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> 
#> → Will install 27 packages.
#> → Will update 1 package.
#> → Will download 27 CRAN packages (41.90 MB).
#> → Will download 1 package with unknown size.
#> + cli                  3.6.5  🔧 ⬇ (1.47 MB)
#> + dplyr                1.2.0  🔧 ⬇ (1.64 MB)
#> + farver               2.1.2  🔧 ⬇ (1.97 MB)
#> + generics             0.1.4   ⬇ (82.60 kB)
#> + ggplot2              4.0.2   ⬇ (8.49 MB)
#> + glue                 1.8.0  🔧 ⬇ (175.14 kB)
#> + gtable               0.3.6   ⬇ (225.12 kB)
#> + isoband              0.3.0  🔧 ⬇ (1.96 MB)
#> + labeling             0.4.3   ⬇ (61.83 kB)
#> + lifecycle            1.0.5   ⬇ (132.98 kB)
#> + magrittr             2.0.4  🔧 ⬇ (231.97 kB)
#> + mstATA       0.1.0 → 0.1.0  👷🏿🔧 ⬇ (GitHub: 1e7caf3)
#> + pillar               1.11.1  ⬇ (660.11 kB)
#> + pkgconfig            2.0.3   ⬇ (18.47 kB)
#> + purrr                1.2.1  🔧 ⬇ (585.69 kB)
#> + R6                   2.6.1   ⬇ (87.28 kB)
#> + RColorBrewer         1.1-3   ⬇ (51.79 kB)
#> + rlang                1.1.7  🔧 ⬇ (1.90 MB)
#> + scales               1.4.0   ⬇ (873.34 kB)
#> + stringi              1.8.7  🔧 ⬇ (14.79 MB)
#> + stringr              1.6.0   ⬇ (333.11 kB)
#> + tibble               3.3.1  🔧 ⬇ (659.83 kB)
#> + tidyr                1.3.2  🔧 ⬇ (1.31 MB)
#> + tidyselect           1.2.1   ⬇ (226.89 kB)
#> + utf8                 1.2.6  🔧 ⬇ (209.74 kB)
#> + vctrs                0.7.1  🔧 ⬇ (2.22 MB)
#> + viridisLite          0.4.3   ⬇ (1.30 MB)
#> + withr                3.0.2   ⬇ (224.91 kB)
#> ℹ Getting 27 pkgs (41.90 MB) and 1 pkg with unknown size
#> ✔ Got R6 2.6.1 (aarch64-apple-darwin20) (87.28 kB)
#> ✔ Got RColorBrewer 1.1-3 (aarch64-apple-darwin20) (51.79 kB)
#> ✔ Got generics 0.1.4 (aarch64-apple-darwin20) (82.60 kB)
#> ✔ Got labeling 0.4.3 (aarch64-apple-darwin20) (61.83 kB)
#> ✔ Got lifecycle 1.0.5 (aarch64-apple-darwin20) (132.98 kB)
#> ✔ Got pkgconfig 2.0.3 (aarch64-apple-darwin20) (18.47 kB)
#> ✔ Got purrr 1.2.1 (aarch64-apple-darwin20) (585.69 kB)
#> ✔ Got cli 3.6.5 (aarch64-apple-darwin20) (1.47 MB)
#> ✔ Got pillar 1.11.1 (aarch64-apple-darwin20) (660.11 kB)
#> ✔ Got dplyr 1.2.0 (aarch64-apple-darwin20) (1.64 MB)
#> ✔ Got farver 2.1.2 (aarch64-apple-darwin20) (1.97 MB)
#> ✔ Got glue 1.8.0 (aarch64-apple-darwin20) (175.14 kB)
#> ✔ Got magrittr 2.0.4 (aarch64-apple-darwin20) (231.97 kB)
#> ✔ Got tidyr 1.3.2 (aarch64-apple-darwin20) (1.31 MB)
#> ✔ Got viridisLite 0.4.3 (aarch64-apple-darwin20) (1.30 MB)
#> ✔ Got scales 1.4.0 (aarch64-apple-darwin20) (873.34 kB)
#> ✔ Got rlang 1.1.7 (aarch64-apple-darwin20) (1.90 MB)
#> ✔ Got withr 3.0.2 (aarch64-apple-darwin20) (224.91 kB)
#> ✔ Got utf8 1.2.6 (aarch64-apple-darwin20) (209.74 kB)
#> ✔ Got gtable 0.3.6 (aarch64-apple-darwin20) (225.12 kB)
#> ✔ Got vctrs 0.7.1 (aarch64-apple-darwin20) (2.22 MB)
#> ✔ Got isoband 0.3.0 (aarch64-apple-darwin20) (1.96 MB)
#> ✔ Got tibble 3.3.1 (aarch64-apple-darwin20) (659.83 kB)
#> ✔ Got stringr 1.6.0 (aarch64-apple-darwin20) (333.11 kB)
#> ✔ Got tidyselect 1.2.1 (aarch64-apple-darwin20) (226.89 kB)
#> ✔ Got mstATA 0.1.0 (source) (821.93 kB)
#> ✔ Got ggplot2 4.0.2 (aarch64-apple-darwin20) (8.49 MB)
#> ✔ Got stringi 1.8.7 (aarch64-apple-darwin20) (14.79 MB)
#> ✔ Installed R6 2.6.1  (76ms)
#> ✔ Installed RColorBrewer 1.1-3  (83ms)
#> ✔ Installed cli 3.6.5  (92ms)
#> ✔ Installed dplyr 1.2.0  (118ms)
#> ✔ Installed farver 2.1.2  (122ms)
#> ✔ Installed generics 0.1.4  (127ms)
#> ✔ Installed ggplot2 4.0.2  (150ms)
#> ✔ Installed glue 1.8.0  (156ms)
#> ✔ Installed gtable 0.3.6  (60ms)
#> ✔ Installed isoband 0.3.0  (38ms)
#> ✔ Installed labeling 0.4.3  (54ms)
#> ✔ Installed lifecycle 1.0.5  (55ms)
#> ✔ Installed magrittr 2.0.4  (39ms)
#> ✔ Installed pillar 1.11.1  (36ms)
#> ✔ Installed pkgconfig 2.0.3  (36ms)
#> ✔ Installed purrr 1.2.1  (36ms)
#> ✔ Installed rlang 1.1.7  (38ms)
#> ✔ Installed scales 1.4.0  (36ms)
#> ✔ Installed stringr 1.6.0  (19ms)
#> ✔ Installed stringi 1.8.7  (105ms)
#> ✔ Installed tibble 3.3.1  (45ms)
#> ✔ Installed tidyr 1.3.2  (37ms)
#> ✔ Installed tidyselect 1.2.1  (38ms)
#> ✔ Installed utf8 1.2.6  (37ms)
#> ✔ Installed vctrs 0.7.1  (37ms)
#> ✔ Installed viridisLite 0.4.3  (162ms)
#> ✔ Installed withr 3.0.2  (171ms)
#> ℹ Packaging mstATA 0.1.0
#> ✔ Packaged mstATA 0.1.0 (1.3s)
#> ℹ Building mstATA 0.1.0
#> ✔ Built mstATA 0.1.0 (4s)
#> ✔ Installed mstATA 0.1.0 (github::Hongchen030/mstATA@1e7caf3) (32ms)
#> ✔ 1 pkg + 30 deps: kept 1, upd 1, added 27, dld 28 (NA B) [14.5s]
```

Alternatively, you can use `remotes`:

``` r
# install.packages("remotes")
remotes::install_github("Hongchen030/mstATA")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'mstATA' from a github remote, the SHA1 (1e7caf33) has not changed since last install.
#>   Use `force = TRUE` to force installation
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
