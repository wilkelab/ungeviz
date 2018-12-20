#' Sample outcomes from a fitted model
#'
#' Currently, sampling of fitted models is only implemented for
#' GAMs fitted with [`mgcv::gam()`].
#'
#' @examples
#' library(mgcv)
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
#' ggplot(mtcars, aes(disp, mpg)) +
#'  geom_point(color = "grey30", size = 0.5) +
#'  geom_line(data = sample_df, aes(group = .draw), color = "#0072B2", size = 0.3)
#'@export
sample_outcomes <- function(model, newdata, samples = 20, ...) {
  UseMethod("sample_outcomes", model)
}

#'@export
sample_outcomes.default <- function(model, ...) {
  stop(
    "Function `sample_outcomes()` not implemented for objects of class '",
    class(model), "'.", call. = FALSE
  )
}

#'@export
sample_outcomes.gam <- function(model, newdata, ndraws = 20, unconditional = FALSE) {
  predictor <- model$pred.formula[[2]]
  response <- model$terms[[2]]

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
  coefs <- mvtnorm::rmvnorm(ndraws, mean = coef(model), sigma = vcov_mat)
  preds <- pred_mat %*% t(coefs)
  pred_df <- as_tibble(preds) %>%
    set_names(as.character(1:ndraws)) %>%
    cbind(newdata) %>%
    gather(.draw, !!response, -!!predictor)

  pred_df
}
