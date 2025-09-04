
# gglyph <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->
[![Dev version](https://img.shields.io/badge/devel%20version-0.0.0.9000-orange.svg)](https://github.com/valentinsvelev/gglyph)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN status](https://www.r-pkg.org/badges/version/gglyph)](https://CRAN.R-project.org/package=gglyph)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/gglyph)](https://CRAN.R-project.org/package=gglyph)
[![R-CMD-check](https://github.com/valentinsvelev/gglyph/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/valentinsvelev/gglyph/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**gglyph** provides tools for creating network-style visualizations of
directed pairwise relationships using custom edge glyphs built on top of
`ggplot2`.

Please note that the package has two licenses:

- [MIT](https://opensource.org/license/mit) for the code
- [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) for the data

## Installation

You can install the development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("valentinsvelev/gglyph")
```

## Use

You can use the package with real data as follows:

``` r
library(gglyph)

data <- process_data_statistical(df)

ggplot(data = data) +
  geom_glpyh()
```

Or by first generating mock data:

``` r
library(gglyph)

data <- generate_mock_data(n_nodes = 5, n_edges = 10)

ggplot(data = data) +
  geom_glpyh()
```

# Citation

``` r
citation("gglyph")
```

To cite the package use:

    Velev, V., & Spitz, A. (2025). gglyph: Network-Style Visualization Of Directed Pairwise Relationships. R package version 0.0.0.9000. https://github.com/valentinsvelev/gglyph

Or for LaTeX users:

    @Manual{,
      title = {{gglyph: Network-Style Visualization Of Directed Pairwise Relationships}},
      author = {Velev, Valentin and Spitz, Andreas},
      year = {2021},
      note = {R package version 0.0.0.9000},
      url = {https://github.com/valentinsvelev/gglyph}
    }
