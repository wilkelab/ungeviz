#' Sample outcomes from a fitted model
#'
#' Currently, sampling of fitted models is only implemented for
#' GAMs fitted with [`mgcv::gam()`].
#'
#' @examples
#' library(mgcv)
#' library(ggplot2)
#' fit <- gam(mpg ~ s(disp), data = mtcars, method = "REML")
#'
#' newdata <- data.frame(
#'   disp = seq(
#'     min(mtcars$disp), max(mtcars$disp),
#'     length.out = 100
#'   )
#' )
#'
#' sample_df <- sample_outcomes(fit, newdata, 10, unconditional = TRUE)
#' conf <- confidence_band(fit, newdata, unconditional = TRUE)
#' ggplot(mtcars, aes(disp, mpg)) +
#'   geom_ribbon(data = conf, aes(ymin = lo, ymax = hi), fill="#80808080", color = NA) +
#'   geom_point(color = "grey30", size = 0.5) +
#'   geom_line(data = sample_df, aes(group = .bootstrap), color = "#0072B2", size = 0.3) +
#'   geom_line(data = conf, size = 1, color = "darkred")
#'
#'
#' data(BlueJays, package = "Stat2Data")
#'
#' fit <- gam(Skull ~ s(BillDepth, BillWidth), data = BlueJays, method = "REML")
#'
#' newdata <- expand.grid(
#'   BillWidth = seq(min(BlueJays$BillWidth), max(BlueJays$BillWidth), length.out = 20),
#'   BillDepth = seq(min(BlueJays$BillDepth), max(BlueJays$BillDepth), length.out = 20)
#' )
#'
#' sample_df <- sample_outcomes(fit, newdata, 9, unconditional = TRUE)
#' ggplot(BlueJays, aes(BillDepth, BillWidth)) +
#'   geom_tile(data = sample_df, aes(fill = Skull), alpha = 0.5, color = NA) +
#'   geom_contour(data = sample_df, aes(z = Skull), color = "black", size = 0.3, binwidth = 0.2) +
#'   geom_point(aes(color = Skull), size = 1.5) +
#'   scale_fill_viridis_c(
#'     name = "skull, modeled (mm)",
#'     option = "D",
#'     guide = guide_legend(
#'       title.position = "top",
#'       order = 2,
#'       override.aes = list(alpha = 0.5)
#'     )
#'   ) +
#'   scale_color_viridis_c(
#'     name = "skull, observed (mm)",
#'     option = "D",
#'     guide = guide_legend(title.position = "top", order = 1)
#'   ) +
#'   coord_cartesian(expand = FALSE) +
#'   xlab("bill depth (mm)") +
#'   ylab("bill width (mm)") +
#'   ggtitle("Variability in 2d spline model fitted to blue jay data") +
#'   facet_wrap(~.bootstrap, labeller = as_labeller(function(x) paste0("posterior sample ", x))) +
#'   theme_bw() +
#'   theme(
#'     legend.direction = "horizontal",
#'     legend.position = "bottom",
#'     legend.justification = "right",
#'     legend.title.align = 0.5
#'   )
#' @export
sample_outcomes <- function(model, newdata, times = 20, ...) {
  UseMethod("sample_outcomes", model)
}

#' @export
sample_outcomes.default <- function(model, ...) {
  stop(
    "Function `sample_outcomes()` not implemented for objects of class '",
    class(model), "'.", call. = FALSE
  )
}

#'@export
sample_outcomes.gam <- function(model, newdata, times = 20, unconditional = FALSE) {
  # original code concept: Noam Ross
  # https://gist.github.com/noamross/8bf1fc5b2f629b3a7e1eb6b4572e8388
  response <- rlang::expr_name(attr(model$terms, "variables")[[2]])

  # Get the linear prediction matrix
  pred_mat <- predict(
    model,
    newdata = newdata,
    type = "lpmatrix",
    unconditional = unconditional
  )

  # Get the variance-covariance matrix of coefficients
  vcov_mat <- vcov(model, unconditional = unconditional)

  # Draw 20 samples from the posterior and make predictions from them
  coefs <- mvtnorm::rmvnorm(times, mean = coef(model), sigma = vcov_mat)
  preds <- pred_mat %*% t(coefs)
  pred_df <- as_tibble(preds) %>%
    set_names(as.character(1:times)) %>%
    cbind(newdata) %>%
    gather(.bootstrap, !!response, 1:times)

  pred_df
}


#' @rdname sample_outcomes
#' @export
confidence_band <- function(model, newdata, level = 0.95, n = 100, ...) {
  UseMethod("confidence_band", model)
}

#' @export
confidence_band.default <- function(model, ...) {
  stop(
    "Function `confidence_band()` not implemented for objects of class '",
    class(model), "'.", call. = FALSE
  )
}

#' @export
confidence_band.gam <- function(model, newdata, level = 0.95, n = 100, unconditional = FALSE) {
  # original code concept: Noam Ross
  # https://gist.github.com/noamross/8bf1fc5b2f629b3a7e1eb6b4572e8388
  response <- as.symbol(rlang::expr_name(attr(model$terms, "variables")[[2]]))

  # normal quantile corresponding to confidence level
  std <- stats::qnorm(level / 2 + 0.5)

  # predict confidence band
  predict(
    model,
    newdata = newdata,
    se.fit = TRUE,
    unconditional = unconditional
  ) %>%
    as_tibble() %>%
    cbind(newdata) %>%
    rename(!!response := fit) %>%
    mutate(
      lo = !!response - std*se.fit,
      hi = !!response + std*se.fit
    )
}
