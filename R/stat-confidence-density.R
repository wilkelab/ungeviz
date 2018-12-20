#' Confidence density distributions generated from estimate and margin of error
#'
#' This stat generates normal densities from provided estimates plus margins
#' of error (at a specified confidence level). It can be used to estimate
#' the confidence density that underlies a given parameter estimate with
#' given margin of error.
#'
#' @param confidence The confidence level used to calculate the `moe` statistic.
#'    This defaults to 0.95 (`moe` corresponds to 95\% confidence interval).
#' @param xlim Numeric vector of two numbers setting the range of x values to be
#'   covered by the confidence density. If not supplied, is taken from the x scale.
#'
#' @section Details:
#'
#' The following aesthetics are understood by this stat (required aesthetics
#' are in bold):
#'  * **`x`**: The estimate whose uncertainty is to be displayed
#'  * **`moe`**: Margin of error
#'  * `confidence`: Confidence level used to calculate the `moe` statistic.
#'    This defaults to 0.95 (`moe` corresponds to 95\% confidence interval).
#'
#' @source
#' Adrian W. Bowman. Graphs for Uncertainty. J. R. Statist. Soc. A 182:1-16, 2018.
#' \url{http://www.rss.org.uk/Images/PDF/events/2018/Bowman-5-Sept-2018.pdf}
#' @examples
#' library(tidyverse)
#'
#' params <- data.frame(
#'   group = letters[1:3],
#'   mean = c(1, 3, 2),
#'   sd = c(.8, .4, .7)
#' )
#' df_data <- mutate(
#'   params,
#'   value = purrr::map2(mean, sd, ~rnorm(250, .x, .y))
#' ) %>% unnest()
#'
#' df_summary <- group_by(df_data, group) %>%
#'   summarize(
#'     mean = mean(value),
#'     sd = sd(value),
#'     moe = sd*1.96
#'   )
#'
#' ggplot(df_summary, aes(x = mean, y = group)) +
#'   stat_confidence_density(aes(moe = moe, fill = stat(ndensity)), height = 0.8, alpha = NA) +
#'   geom_point(data = df_data, aes(x = value), position = position_jitter(width = 0), size = 0.5) +
#'   geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd), height = 0.2, color = "darkred", size = 1) +
#'   geom_point(size = 3, color = "darkred") +
#'   scale_fill_gradient(low = "#132B4300", high = "#56B1F7FF") +
#'   theme_minimal()
#'
#' library(ggridges)
#'
#' ggplot(df_summary, aes(x = mean, y = group)) +
#'   stat_confidence_density(
#'     geom = "ridgeline",
#'     aes(moe = moe, height = stat(density)),
#'     alpha = NA, xlim = c(-1, 4)
#'   ) +
#'   theme_minimal()
#' @export
stat_confidence_density <- function(mapping = NULL, data = NULL,
                            geom = "tile", position = "identity",
                            ...,
                            confidence = 0.95,
                            xlim = NULL,
                            n = 501,
                            na.rm = FALSE,
                            show.legend = FALSE,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatConfidenceDensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      confidence = confidence,
      n = n,
      na.rm = na.rm,
      xlim = xlim,
      ...
    )
  )
}

#' @rdname stat_confidence_density
#' @usage NULL
#' @format NULL
#' @export
StatConfidenceDensity <- ggproto("StatConfidenceDensity", Stat,
  required_aes = c("x", "moe"),
  default_aes = aes(
    alpha = stat(ndensity),
    confidence = 0.95
  ),

  compute_group = function(data, scales, xlim = NULL, n = 501, confidence = 0.95) {
    # assume confidence level is 0.95 if not provided
    if (is.null(data$confidence)) {
      data$confidence <- confidence
    }

    # Check that confidence density parameters are constant within group
    params <- unique(data[c("x", "moe", "confidence")])
    if (nrow(params) > 1) {
      stop("Confidence density parameters can not vary within data groups", call. = FALSE)
    }
    params <- as.list(params)

    range <- xlim %||% scales$x$dimension()
    xseq <- seq(range[1], range[2], length.out = n)

    if (scales$x$is_discrete()) {
      x_trans <- xseq
    } else {
      # For continuous scales, need to back transform from transformed range
      # to original values
      x_trans <- scales$x$trans$inverse(xseq)
      params$x <- scales$x$trans$inverse(params$x)
      params$statistic <- scales$x$trans$inverse(params$statistic)
    }

    fun <- do.call(fit_normal, params)
    density <- fun(x_trans)

    data.frame(
      x = xseq,
      density = density,
      ndensity = density/max(density)
    )
  }
)


fit_normal <- function(x, moe, confidence) {
  # convert to two-tailed value
  confidence <- 1-(1-confidence)/2
  function(z) dnorm(z, mean = x, sd = moe/qnorm(confidence))
}
