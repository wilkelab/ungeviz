#' Create a sampler function useful for repeated reproducible sampling
#'
#' @param times Number of independent sampling draws to perform.
#' @param size Sample size for each random sample.
#' @param replace Bool indicating whether sampling should occur with or without replacement.
#' @param group An optional expression setting up the grouping to use for bootstrapping.
#'   If not provided, any grouping present in the original dataset will be used.
#' @param seed Random seed to use.
#' @param key Name (as character) of the column that will hold an integer
#'   running from 1 to `times` indicating the bootstrap replicate.
#' @param row Name (as character) of the column that will hold an integer
#'   counting rows in the final bootstrapped dataset. Useful for
#'   animations with gganimate.
#' @param id Name (as character) of the column that will hold an integer
#'   running from 1 to n for each bootstrap, where n is the number
#'   of observations in each group.
#' @param original_id Name (as character) of the column that indicates the
#'   row from which the sampled row originates.
#' @seealso
#' [`bootstrapper()`]
#' @examples
#' spl <- sampler(3, 2)
#' spl(data.frame(letter = letters[1:4]))
#'
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(aes(color = Species), alpha = 0.3) +
#'   geom_point(
#'     data = sampler(1, 5, group = Species),
#'     aes(fill = Species),
#'     color = "black", shape = 21
#'   ) +
#'   theme_bw()
#'
#' # it is important to set grouping correctly for sampling
#' set.seed(1234)
#'
#' df <- data.frame(
#'   type = c(rep("A", 100), rep("B", 10), rep("C", 3)),
#'   y = rnorm(113)
#' )
#'
#' # incorrect: sampling ungrouped dataset leads to missing data
#' # in some categories
#' ggplot(df, aes(type, y)) +
#'   geom_pointrange(data = sampler(6, 3, replace = TRUE, seed = 559), stat = "summary") +
#'   facet_wrap(~.draw)
#'
#' # correct: sampling within groups
#' ggplot(df, aes(type, y)) +
#'   geom_pointrange(data = sampler(6, 3, replace = TRUE, group = type, seed = 559), stat = "summary") +
#'   facet_wrap(~.draw)
#'
#' # also correct: use grouped data frame
#' ggplot(group_by(df, type), aes(type, y)) +
#'   geom_pointrange(data = sampler(6, 3, replace = TRUE, seed = 559), stat = "summary") +
#'   facet_wrap(~.draw)
#'
#' \dontrun{
#' library(gganimate)
#'
#' p <- ggplot(iris, aes(Sepal.Length, Species, color = Species)) +
#'   geom_point(color = "grey50", alpha = 0.3, size = 2) +
#'   geom_point(data = sampler(20, 1, group = Species), size = 4) +
#'   scale_color_brewer(type = "qual", palette = 2, guide = "none") +
#'   theme_bw()
#'
#' p + facet_wrap(~.draw)
#' p + transition_states(.draw, 1, 2)
#' }
#' @export
sampler <- function(times, size = 1, replace = FALSE, group = NULL, seed = NULL,
                    key = ".draw", row = ".row", id = ".id", original_id = ".original_id") {
  force(times)
  force(size)
  force(replace)
  force(key)
  force(row)
  force(id)
  force(original_id)
  group <- enquo(group)

  if (is.null(seed)) {
    seed <- sample(2^31-1, 1)
  }

  sampling_fun <- function(x) {
    map_dfr(
      1:times,
      ~{sample_n(x, size, replace = replace) %>%
          mutate(
            !!key := .x,
            !!id := as.integer(1:size)
          )
      }
    )
  }


  function(.data) {
    with_seed(
      seed,
      {
        if (!quo_is_null(group)) {
          # if group is given, set up new grouping variable
          sample_group <- eval_tidy(group, data = .data)
          .data <- mutate(.data, .draw_group = sample_group) %>%
            group_by(.draw_group)
        }

        .data %>%
#          do(sampling_fun(.)) %>%
#          # would want to replace the `do` line with something like the following:
          samplify(times = times, size = size, replace = replace, key = key) %>%
          #collect(id = id) %>%
          collect(id = id, original_id = original_id) %>%
          mutate(
            !!row := 1
          ) -> out

        # need to set row numbers outside of `mutate()`
        # to circumvent grouping
        out[row] <- as.integer(1:nrow(out))
        out
      }
    )
  }
}

