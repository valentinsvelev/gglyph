
# gglyph: Network-Style Visualization of Directed Pairwise Relationships <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gglyph)](https://CRAN.R-project.org/package=gglyph)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/gglyph)](https://CRAN.R-project.org/package=gglyph)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/valentinsvelev/gglyph/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/valentinsvelev/gglyph/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/valentinsvelev/gglyph/branch/main/graph/badge.svg)](https://app.codecov.io/gh/valentinsvelev/gglyph?branch=main)
<!-- badges: end -->

`gglyph` provides tools for creating network-style visualizations of
directed pairwise relationships using custom edge glyphs built on top of
`ggplot2`.

The package includes four functions:

1.  `geom_glyph()`: Create a network-based graph that illustrates
    pairwise relationships (statistical and non-statistical) using
    custom edges
2.  `process_data_statistical()`: Process statistical data (e.g.,
    pairwise t-tests) for plotting
3.  `process_data_general()`: Process general / non-statistical data
    (any data with directional relationships) for plotting
4.  `generate_mock_data()`: Create mock data for experimenting with
    `geom_glyph()`

They should be used in the following order: either 4 → 1 or 2/3 → 1.

Please note that the package has two licenses:

- [MIT](https://opensource.org/license/mit) for the code
- [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) for the data

## Installation

You can either install the package from CRAN:

``` r
install.packages("gglyph")
```

Or you can install the development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("valentinsvelev/gglyph")
```

## Use

You can use the package as follows:

``` r
library(gglyph)
library(ggplot2)

data <- generate_mock_data(n_nodes = 5, n_edges = 15, statistical = TRUE)

ggplot(data = data) +
  geom_glyph(
    edge_fill = "purple",
    edge_colour = "black",
    node_fill = "lightblue",
    node_colour = "black"
  )
```

<img src="man/figures/README-fig1-1.png" width="100%" />

Or with non-statistical data:

``` r
library(gglyph)
library(ggplot2)

data <- generate_mock_data(n_nodes = 5, n_edges = 10)

ggplot(data = data) +
  geom_glyph(
    edge_fill = "yellow",
    node_fill = "black",
    node_colour = "yellow"
  )
```

<img src="man/figures/README-fig2-1.png" width="100%" />

You can also use `ggplot2`’s `facet_wrap()` for grouped data:

``` r
library(gglyph)
library(ggplot2)
library(ggsci)

data <- generate_mock_data(n_nodes = 5, n_edges = 20, n_groups = 6)

ggplot(data = data) +
  geom_glyph(
    edge_fill = pal_uchicago(),
    node_fill = "black",
    node_colour = pal_uchicago()
  ) +
  facet_wrap(~ group)
```

<img src="man/figures/README-fig3-1.png" width="100%" />

## Citation

To cite the package use:

    Velev, V., & Spitz, A. (2025). gglyph: Network-Style Visualization Of Directed Pairwise Relationships. R package version 0.2.0. https://github.com/valentinsvelev/gglyph

Or for LaTeX users:

    @Manual{,
      title = {{gglyph: Network-Style Visualization Of Directed Pairwise Relationships}},
      author = {Velev, Valentin and Spitz, Andreas},
      year = {2025},
      note = {R package version 0.2.0},
      url = {https://github.com/valentinsvelev/gglyph}
    }
