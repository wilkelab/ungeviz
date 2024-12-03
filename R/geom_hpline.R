#' Draw point-like short line segments
#'
#' The geoms `geom_hpline()` and `geom_vpline()` can be used as a drop-in
#' replacement for [`geom_point()`] but draw horizontal or vertical lines
#' (point-lines, or plines) instead of points. These lines can often be useful to
#' indicate specific parameter estimates in a plot. The geoms take position
#' aesthetics as `x` and `y` like [`geom_point()`], and they use `width` or `height`
#' to set the length of the line segment. All other aesthetics (`colour`, `size`,
#' `linetype`, etc.) are inherited from [`geom_segment()`].
#' @inheritParams ggplot2::geom_point
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_hpline(stat = "summary")
#'
#' ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_point(position = "jitter", size = 0.5) +
#'   stat_summary(aes(colour = Species), geom = "hpline", width = 0.6, size = 1.5)
#'
#' ggplot(iris, aes(Sepal.Length, Species, color = Species)) +
#'   geom_point(color = "grey50", alpha = 0.3, size = 2) +
#'   geom_vpline(data = sampler(5, 1, group = Species), height = 0.4) +
#'   scale_color_brewer(type = "qual", palette = 2, guide = "none") +
#'   facet_wrap(~.draw) +
#'   theme_bw()
#' @export
geom_hpline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHpline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_hpline
#' @format NULL
#' @usage NULL
#' @export
GeomHpline <- ggproto("GeomHpline", GeomSegment,
  required_aes = c("x", "y"),
  non_missing_aes = c("linewidth", "colour", "linetype", "width"),
  default_aes = aes(
    width = 0.5, colour = "black", linewidth = 2, linetype = 1,
    alpha = NA
  ),

  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data <- mutate(data, x = x - width/2, xend = x + width, yend = y)
    ggproto_parent(GeomSegment, self)$draw_panel(
      data, panel_params, coord, arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)

#' @rdname geom_hpline
#' @export
geom_vpline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVpline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_hpline
#' @format NULL
#' @usage NULL
#' @export
GeomVpline <- ggproto("GeomVpline", GeomSegment,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "colour", "linetype", "height"),
  default_aes = aes(
    height = 0.5, colour = "black", size = 2, linetype = 1,
    alpha = NA
  ),

  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data <- mutate(data, y = y - height/2, yend = y + height, xend = x)
    ggproto_parent(GeomSegment, self)$draw_panel(
      data, panel_params, coord, arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)
