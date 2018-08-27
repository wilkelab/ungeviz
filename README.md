
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ungeviz

Tools for visualizing uncertainty with ggplot2, written by Claus O.
Wilke

The name comes from the German word “Ungewissheit” which means
uncertainty.

## Installation

``` r
devtools::install_github("clauswilke/ungeviz")
```

## Examples

``` r
library(broom)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 3.5.1
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(ungeviz)

df_model <- lm(mpg ~ disp + hp + qsec, data = mtcars) %>%
  tidy() %>%
  filter(term != "(Intercept)")

ggplot(df_model, aes(estimate = estimate, moe = std.error, y = term)) +
  stat_conf_strip(fill = "lightblue", height = 0.8) +
  geom_point(aes(x = estimate), size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.5) +
  scale_alpha_identity() +
  xlim(-2, 1)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
