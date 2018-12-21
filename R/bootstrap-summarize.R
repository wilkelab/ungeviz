#' Bootstrapped actions on data frames
#'
#' Perform bootstrapped actions on data frames (tibbles) respecting any
#' grouping that has been set up. The function `bootstrap_summarize()` is
#' a drop-in replacement for [`dplyr::summarize()`] and the function
#' `bootstrap_do()` is a drop-in replacement for [`dplyr::do()`]. The
#' function `bootstrap()` simply generates bootstrapped data sets and
#' returns them in a combined table.
#'
#' @param .data Data frame on which to operate.
#' @param times Number of independent bootstrap draws to perform.
#' @param ... Other arguments handed off to [`summarize()`], [`do()`],
#'   or [`collect()`].
#' @param key Name of the column that will hold an integer
#'   running from 1 to `times` indicating the bootstrap replicate.
#' @examples
#' iris %>% group_by(Species) %>%
#'   bootstrap_summarize(3, mean_sepal_length = mean(Sepal.Length))
#'
#' data(BlueJays, package = "Stat2Data")
#' BlueJays %>% group_by(KnownSex) %>%
#'   bootstrap_do(
#'     5,
#'     broom::tidy(lm(BillLength ~ Head, data = .))
#'   )
#' @export
bootstrap_summarize <- function(.data, times, ..., key = ".bootstrap") {
  args <- enquos(...)
  key <- enquo(key)

  summarize(
    bootstrapify(.data, times, key = !!key),
    !!!args
  )
}

#' @rdname bootstrap_summarize
#' @export
bootstrap_summarise <- bootstrap_summarize

#' @rdname bootstrap_summarize
#' @export
bootstrap_do <- function(.data, times, ..., key = ".bootstrap") {
  args <- enquos(...)
  key <- enquo(key)

  do(
    bootstrapify(.data, times, key = !!key),
    !!!args
  )
}

#' @rdname bootstrap_summarize
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' mtcars %>% bootstrap_collect(20) %>%
#'   ggplot(aes(hp, mpg)) +
#'   geom_point(data = mtcars) +
#'   geom_smooth(aes(group = .bootstrap), method = "lm", se = FALSE)
#'
#' \dontrun{
#' library(gganimate)
#' mtcars %>% bootstrap_collect(20) %>%
#'   ggplot(aes(hp, mpg)) +
#'   geom_point(data = mtcars) +
#'   geom_smooth(aes(group = .bootstrap), method = "lm", se = FALSE) +
#'   transition_states(.bootstrap, 1, 1) +
#'   shadow_mark(color = "gray60", size = 0.3)
#' }
#' @export
bootstrap_collect <- function(.data, times, ..., key = ".bootstrap") {
  key <- enquo(key)

  key <- enquo(key)

  collect(
    bootstrapify(.data, times, key = !!key),
    ...
  )
}

#' @rdname bootstrap_summarize
#' @usage NULL
#' @export
bootstrap <- function(...) {
  warning("Function ungeviz::bootstrap() is deprecated. Use ungeviz::bootstrap_collect().", call. = FALSE)
  bootstrap_collect(...)
}


