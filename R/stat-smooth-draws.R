# file based on stat-smooth.R from ggplot2

#' Generate outcome draws from a smooth fit
#'
#' Generate outcome draws from a smooth fit. This stat is similar to [`stat_smooth()`],
#' but there are a few important differences. First, there is no `method` argument.
#' Only smooth fits fitted via [`mgcv::gam()`] are currently supported. If you want a
#' linear fit, set a linear formula via `formula = y ~ x`. Second, there is no `se`
#' argument. This stat cannot draw confidence bands. See [`confidence_band()`] for a
#' workaround if you want to add confidence bands. Internally, the stat uses the
#' function [`sample_outcomes()`] to calculate outcomes.
#'
#' This stat fits the gam with Restricted Maximum Likelihood (REML) and uses the
#' smoothing parameter uncertainty corrected covariance matrix to generate outcomes
#' (`unconditional = TRUE` in [`sample_outcomes()`]). If you choose a different gam
#' fitting method the stat sets `unconditional = FALSE`.
#'
#' Note that for static plots, you will generally have to set the `group`
#' aesthetic appropriately (e.g., `aes(group = stat(.draw))`). However, for
#' animated plots you will normally not want to set the group aesthetic in
#' this way. To enable animations by default, `stat_smooth_draws()` does not
#' set a group aesthetic. See examples for further details.
#'
#' @inheritParams ggplot2::stat_smooth
#' @param times Number of outcomes to draw.
#' @param formula Formula to use in smoothing function. Default is
#'   a cubic spline, `y ~ s(x, bs = "cs")`. To generate a linear fit,
#'   set `formula = y ~ x`.
#' @param gam.args List of additional arguments passed on to the
#'   GAM call.
#' @examples
#' library(ggplot2)
#'
#' # static plots, need to set group aesthetic manually
#' ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   stat_smooth_draws(aes(group = stat(.draw)), size = 0.5) +
#'   theme_bw()
#'
#' # if we want to group by multiple variables, we have to use their
#' # mapped name (here, `colour` instead of `Species`) because we're
#' # creating the groups after after initial data mapping
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   stat_smooth_draws(
#'     formula = y ~ x,
#'     aes(group = interaction(stat(.draw), colour)),
#'     size = 0.5
#'   ) +
#'   theme_bw()
#'
#' \dontrun{
#'
#' # animated plots
#' library(gganimate)
#'
#' ggplot(mtcars, aes(hp, mpg)) +
#'   #geom_point() +
#'   stat_smooth_draws(size = 0.5) +
#'   transition_states(stat(.draw), 1, 2)
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   #geom_point() +
#'   stat_smooth_draws(formula = y ~ x, times = 20, size = 0.5) +
#'   transition_states(stat(.draw), 1, 2)
#' }
#' @export
stat_smooth_draws <- function(mapping = NULL, times = 10,
                              data = NULL,
                              geom = "smooth", position = "identity",
                              ...,
                              formula = y ~ s(x, bs = "cs"),
                              n = 80,
                              fullrange = FALSE,
                              gam.args = list(method = "REML"),
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothdraws,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      formula = formula,
      n = n,
      fullrange = fullrange,
      na.rm = na.rm,
      gam.args = gam.args,
      ...
    )
  )
}

#' @rdname stat_smooth_draws
#' @format NULL
#' @usage NULL
#' @export
StatSmoothdraws <- ggproto("StatSmoothdraws", Stat,
  # Setting the group aesthetic by default is good for static plots but
  # bad for animations. We keep it unset by default to make animations
  # easier.
  #default_aes = aes(group = stat(.draw)),

  compute_group = function(data, scales, times = 10, formula = y ~ s(x, bs = "cs"),
                           se = FALSE, n = 80, fullrange = FALSE,
                           xseq = NULL, level = 0.95, gam.args = list(method = "REML"),
                           na.rm = FALSE) {
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(new_data_frame())
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }


    #base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
    base.args <- list(quote(formula), data = quote(data))
    model <- do.call(mgcv::gam, c(base.args, gam.args))

    unconditional <- FALSE
    if (gam.args$method == "REML") {
      unconditional <- TRUE
    }

    sample_outcomes(model, data.frame(x = xseq), times = times, unconditional = unconditional)
  },

  required_aes = c("x", "y")
)
