#' Summarize data on bootstrap replicates
#'
#' Summarize data on bootstrap replicates
#'
#' @usage NULL
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(cyl, mpg, color = factor(stat(.draw)))) +
#'   stat_summary_boot(fun.y = "mean", geom = "point")
#'
#' \dontrun{
#' library(gganimate)
#' ggplot(mtcars, aes(cyl, mpg)) +
#'   stat_summary_boot(
#'     aes(color = factor(stat(.draw))),
#'     fun.y = "mean", geom = "point", size = 2
#'   ) +
#'   transition_states(stat(.draw), 1, 1)
#'
#' # doesn't work
#' ggplot(mtcars, aes(cyl, mpg)) +
#'   geom_point() +
#'   stat_summary_boot(
#'     aes(color = factor(stat(.draw))),
#'     fun.y = "mean", geom = "point", size = 2
#'   ) +
#'   transition_states(stat(.draw), 1, 1)
#'  }
#' @export
stat_summary_boot <- function(mapping = NULL, data = NULL,
                         geom = "pointrange", position = "identity",
                         ...,
                         fun.data = NULL,
                         fun.y = NULL,
                         fun.ymax = NULL,
                         fun.ymin = NULL,
                         fun.args = list(),
                         ndraws = 5,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryBoot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      fun.y = fun.y,
      fun.ymax = fun.ymax,
      fun.ymin = fun.ymin,
      fun.args = fun.args,
      ndraws = ndraws,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat_summary_boot
#' @usage NULL
#' @format NULL
#' @export
StatSummaryBoot <- ggproto("StatSummaryBoot", StatSummary,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    data
  },

  compute_panel = function(data, scales, fun.data = NULL, fun.y = NULL,
                           fun.ymax = NULL, fun.ymin = NULL, fun.args = list(),
                           ndraws = 5, na.rm = FALSE) {
    fun <- ggplot2:::make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)

    n <- nrow(data)
    final <- data.frame()
    for (i in 1:ndraws) {
      id <- sample(n, n, replace = TRUE)
      d <- ggplot2:::summarise_by_x(data[id, ], fun)
      d$.draw <- i
      final <- rbind(final, d)
    }
    final
  }
)
