#' Summarize data on bootstrap replicates
#' @usage NULL
#' @export
stat_summary_boot <- function(mapping = NULL, data = NULL,
                         geom = "pointrange", position = "identity",
                         ...,
                         fun.data = NULL,
                         fun.y = NULL,
                         fun.ymax = NULL,
                         fun.ymin = NULL,
                         fun.args = list(),
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
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat-summary-boot
#' @usage NULL
#' @export
StatSummaryBoot <- ggproto("StatSummaryBoot", StatSummary,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    data
  },

  compute_panel = function(data, scales, fun.data = NULL, fun.y = NULL,
                           fun.ymax = NULL, fun.ymin = NULL, fun.args = list(),
                           na.rm = FALSE) {
    fun <- ggplot2:::make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)

    reps <- 5
    n <- nrow(data)
    final <- data.frame()
    for (i in 1:reps) {
      id <- sample(n, n, replace = TRUE)
      d <- ggplot2:::summarise_by_x(data[id, ], fun)
      d$replicate <- i
      final <- rbind(final, d)
    }
    final
  }
)
