
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ungeviz

Tools for visualizing uncertainty with ggplot2, written by Claus O.
Wilke

This package is meant to provide helpful add-on functionality for
ggplot2 to visualize uncertainty. The package is particularly focused on
hypothetical outcome plots (HOPs) and provides bootstrapping and
sampling functionality that integrates well with the ggplot2 API.

The package name comes from the German word “Ungewissheit”, which means
uncertainty.

## Installation

``` r
devtools::install_github("clauswilke/ungeviz")
```

## Sampling and bootstrapping

The `sampler()` and `bootstrapper()` functions generate sampling and
bootstrapping objects, respectively, that can be used in place of data
in ggplot2 layers. These objects are helpful when creating HOPs.

``` r
library(tidyverse)
library(ungeviz)
library(gganimate)

cacao %>% filter(location %in% c("Canada", "U.S.A.")) %>%
  ggplot(aes(rating, location)) +
  geom_point(
    position = position_jitter(height = 0.3, width = 0.05), 
    size = 0.4, color = "#0072B2", alpha = 1/2
  ) +
  geom_vpline(data = sampler(25, group = location), height = 0.6, color = "#D55E00") +
  theme_bw() + 
  # `.draw` is a generated column indicating the 
  # sample draw to which a data row belongs
  transition_states(.draw, 1, 3)
```

![](man/figures/README-cacao-samples-anim-1.gif)<!-- -->

``` r
ggplot(mtcars, aes(mpg, hp)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = 3), color = NA, alpha = 0.3) + 
  geom_point() +
  geom_smooth(
    data = bootstrapper(20),
    method = "gam", formula = y ~ s(x, k = 3), se = FALSE, fullrange = TRUE
  ) + 
  theme_bw() +
  transition_states(.draw, 1, 2)
```

![](man/figures/README-mtcars-smooth-anim-1.gif)<!-- -->

Both the bootstrapper and sampler objects can be used for repeated
reproducible sampling.

``` r
data(BlueJays, package = "Stat2Data")

# set up bootstrapping object that generates 20 bootstraps
# and groups by variable `KnownSex`
bs <- bootstrapper(20, KnownSex)

ggplot(BlueJays, aes(BillLength, Head, color = KnownSex)) +
  geom_smooth(method = "lm", color = NA) +
  geom_point(alpha = 0.3) +
  # `.row` is a generated column providing a unique row number
  # to all rows in the bootstrapped data frame 
  geom_point(data = bs, aes(group = .row)) +
  geom_smooth(data = bs, method = "lm", fullrange = TRUE, se = FALSE) +
  facet_wrap(~KnownSex, scales = "free_x") +
  scale_color_manual(values = c(F = "#D55E00", M = "#0072B2"), guide = "none") +
  theme_bw() +
  transition_states(.draw, 1, 1) + 
  enter_fade() + 
  exit_fade()
```

![](man/figures/README-bluejays-lm-anim-1.gif)<!-- -->

## Visualizing uncertainty from fitted models

The new stat `stat_confidence_density()` can be used to generate
confidence strips.

``` r
library(broom)
#> 
#> Attaching package: 'broom'
#> The following object is masked from 'package:ungeviz':
#> 
#>     bootstrap
library(emmeans)

cacao_lumped <- cacao %>%
  mutate(
    location = fct_lump(location, n = 20)
  )
  
cacao_means <- lm(rating ~ location, data = cacao_lumped) %>%
  emmeans("location") %>%
  tidy() %>%
  mutate(location = fct_reorder(location, estimate))

ggplot(cacao_means, aes(x = estimate, moe = std.error, y = location)) +
  stat_confidence_density(fill = "lightblue", height = 0.8, confidence = 0.68) +
  geom_point(aes(x = estimate), size = 2) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.5) +
  scale_alpha_identity() +
  xlim(2.6, 3.7) +
  theme_minimal()
```

![](man/figures/README-cacao-means-1.png)<!-- -->
