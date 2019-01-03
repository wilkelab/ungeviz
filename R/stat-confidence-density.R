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
#' cacao_small <- cacao %>%
#'   filter(location %in% c("Switzerland", "Canada", "U.S.A.", "Belgium"))
#'
#' cacao_summary <- cacao_small %>%
#'   group_by(location) %>%
#'   summarize(
#'     sd = sd(rating),
#'     moe = sd*1.96,
#'     rating = mean(rating)
#'   )
#'
#' ggplot(cacao_summary, aes(x = rating, y = location)) +
#'   stat_confidence_density(aes(moe = moe, fill = stat(ndensity)), height = 0.8) +
#'   geom_point(data = cacao_small, position = position_jitter(width = 0.05), size = 0.3) +
#'   geom_errorbarh(aes(xmin = rating - sd, xmax = rating + sd), height = 0.3, color = "darkred", size = 1) +
#'   geom_point(size = 3, color = "darkred") +
#'   theme_minimal()
#'
#'
#' library(ggridges)
#'
#' cacao_se <- cacao_small %>%
#'   group_by(location) %>%
#'   summarize(
#'     se = sd(rating)/sqrt(n()),
#'     moe = se*1.96,
#'     rating = mean(rating)
#'   )
#'
#' ggplot(cacao_se, aes(x = rating, y = location)) +
#'   stat_confidence_density(
#'     geom = "ridgeline",
#'     aes(moe = moe, height = stat(density)),
#'     alpha = NA, xlim = c(2.5, 3.75), scale = 0.08
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
  l <- layer(
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

  list(l, scale_alpha_identity())
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
