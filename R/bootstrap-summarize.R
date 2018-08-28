#' Bootstrapped actions on data frames
#'
#' Perform bootstrapped actions on data frames (tibbles) respecting any
#' grouping that has been set up. The function `bootstrap_summarize()` is
#' a drop-in replacement for `[dplyr::summarize()]` and the function
#' `bootstrap_do()` is a drop-in replacement for `[dplyr::do()]`. The
#' function `bootstrap()` simply generates bootstrapped data sets and
#' returns them in a combined table.
#'
#' @param .data Data frame on which to operate.
#' @param ndraws Number of independent bootstrap draws to perform.
#' @param ... Other arguments handed off to `[summarize()]` or `[do()]`.
#' @param .draw_key Name of the column that will hold an integer
#'   running from 1 to `ndraws` indicating the bootstrap replicate.
#' @export
bootstrap_summarize <- function(.data, ndraws, ..., .draw_key = ".draw") {
  args <- enquos(...)

  resample_sum <- function(.data, argquos, i) {
    n <- nrow(.data)
    ids <- sample(n, n, replace = TRUE)
    summarize(.data[ids, , drop = FALSE], !!.draw_key := i, !!!argquos)
  }

  do(
    .data,
    .draw_df = map_dfr(1:ndraws, function(i) resample_sum(., args, i))
  ) %>% unnest()
}

#' @rdname bootstrap_summarize
#' @export
bootstrap_summarise <- bootstrap_summarize

#' @rdname bootstrap_summarize
#' @export
bootstrap_do <- function(.data, ndraws, ..., .draw_key = ".draw") {
  args <- enquos(...)

  resample_do <- function(.data, argquos, i) {
    n <- nrow(.data)
    ids <- sample(n, n, replace = TRUE)
    out <- do(.data[ids, , drop = FALSE], !!!argquos)
    cbind(tibble(!!.draw_key := rep(i, nrow(out))), out)
  }

  do(
    .data,
    .draw_df = map_dfr(1:ndraws, function(i) resample_do(., args, i))
  ) %>% unnest()
}

#' @rdname bootstrap_summarize
#' @export
bootstrap <- function(.data, ndraws, .draw_key = ".draw") {
  bootstrap_do(
    .data,
    ndraws = ndraws,
    out = {.},
    .draw_key = .draw_key
  ) %>%
    select(out, !!.draw_key) %>%
    unnest() %>%
    group_by(!!as.symbol(.draw_key))
}





