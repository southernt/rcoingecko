
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcoingecko <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rcoingecko)](https://CRAN.R-project.org/package=rcoingecko)
<!-- badges: end -->

R Package for Querying Cryptocurrency Data from the [CoinGecko
API](https://www.coingecko.com/api/documentations/v3)

------------------------------------------------------------------------

## Installation

You can install the development version of rcoingecko from
[GitHub](https://github.com/southernt/rcoingecko) with:

``` r
# install.packages("devtools")
devtools::install_github("southernt/rcoingecko")
```

## Example

``` r
library(rcoingecko)
## basic example code
```

``` r
gg_stablecoins_pie()
```

<img src="man/figures/README-stablecoins-1.png" width="100%" />

``` r
gg_crypto_platform_prop()
```

<img src="man/figures/README-crypto_platforms-1.png" width="100%" />

------------------------------------------------------------------------

## Existing Packages

-   [CoinGeckoR](https://github.com/SamBuckberry/CoinGeckoR)
-   [geckor](https://github.com/next-game-solutions/geckor)
