#' Horizontal confidence strips generated from statistical models
#'
#' The following aesthetics are understood by this stat (required aesthetics
#' are in bold):
#'  * **`estimate`**: The estimate whose uncertainty is to be displayed
#'  * **`moe`**: Margin of error
#'  * `confidence`: Confidence level of the moe, defaults to 0.68 (one standard deviation)
#'
#' @param confidence The confidence level used to calculate the `moe` statistic.
#'   If the standard error is used as `moe`, then the confidence should be 0.68
#'   (the default).
#' @param xlim Numeric vector of two numbers setting the range of x values to be
#'   covered by the confidence strip. If not supplied, is taken from the x scale.
#' @export
stat_conf_strip <- function(mapping = NULL, data = NULL,
                            geom = "tile", position = "identity",
                            ...,
                            confidence = 0.68,
                            xlim = NULL,
                            n = 501,
                            na.rm = FALSE,
                            show.legend = FALSE,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatConfStrip,
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

#' @rdname stat_conf_strip
#' @usage NULL
#' @export
StatConfStrip <- ggproto("StatConfStrip", Stat,
  required_aes = c("estimate", "moe"),
  default_aes = aes(
    x = stat(x),
    alpha = stat(ndensity)
  ),

  compute_group = function(data, scales, xlim = NULL, n = 501, confidence = 0.68) {
    # assume infinite degrees of freedom if not provided
    if (is.null(data$confidence)) {
      data$confidence <- confidence
    }

    # Check that confidence band parameters are constant within group
    params <- unique(data[c("estimate", "moe", "confidence")])
    if (nrow(params) > 1) {
      stop("Confidence strip parameters can not vary within data groups", call. = FALSE)
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


fit_normal <- function(estimate, moe, confidence) {
  # convert to two-tailed value
  confidence <- 1-(1-confidence)/2
  function(x) dnorm(x, mean = estimate, sd = moe/qnorm(confidence))
}
