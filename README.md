
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

## Visualizing uncertainty from fitted models

``` r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.0.0.9000     ✔ purrr   0.2.5     
#> ✔ tibble  1.4.2          ✔ dplyr   0.7.6     
#> ✔ tidyr   0.8.0          ✔ stringr 1.3.1     
#> ✔ readr   1.1.1          ✔ forcats 0.3.0
#> Warning: package 'dplyr' was built under R version 3.5.1
#> ── Conflicts ──────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(broom)
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

## Bootstrapping

The functions `bootstrap_summarize()` and `bootstrap_do()` are drop-in
equivalents for dplyr’s `summarize()` and `do()` but perform the
respective action multiple times on bootstrapped data. This is
convenient to generate hypothetical outcomes plots.

``` r
library(gganimate)
diamonds %>% group_by(cut, color, clarity) %>%
  bootstrap_summarize(20, mean_price = mean(price)) %>%
  ggplot(aes(color, mean_price, color = clarity)) +
  geom_point() + facet_wrap(~cut) +
  transition_states(.draw, 1, 1)
```

![](man/figures/README-unnamed-chunk-4-1.gif)<!-- -->

``` r
data(BlueJays, package = "Stat2Data")

BlueJays %>% group_by(KnownSex) %>%
  bootstrap_do(20,
    tidy(lm(BillWidth ~ BillDepth, data = .))
  ) %>%
  select(KnownSex, .draw, term, estimate) %>%
  spread(term, estimate) %>%
  ggplot(aes(BillDepth, BillWidth)) +
    geom_point(data = BlueJays, color = "#0072B2") +
    geom_smooth(data = BlueJays, method = "lm", color = NA) +
    geom_abline(aes(slope = BillDepth, intercept = `(Intercept)`)) +
    facet_wrap(~KnownSex, scales = "free_x") +
    transition_states(.draw, 1, 1)
```

![](man/figures/README-unnamed-chunk-5-1.gif)<!-- -->
