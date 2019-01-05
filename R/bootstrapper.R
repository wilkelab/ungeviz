#' Create a bootstrapper function useful for repeated reproducible bootstrapping
#'
#' @param times Number of independent bootstrap draws to perform.
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
#'   row from which the bootstrapped row originates.
#' @param copies Name (as character) of the column that reports how often
#'   a specific original row has been oversampled.
#' @seealso
#' [`sampler()`]
#' @examples
#' bs <- bootstrapper(3)
#' bs(data.frame(letter = letters[1:4]))
#'
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   geom_smooth(data = bootstrapper(5, Species), aes(group = .draw), se = FALSE) +
#'   facet_wrap(~Species)
#'
#' # it is important to set grouping correctly for bootstrapping
#' set.seed(1234)
#'
#' df <- data.frame(
#'   type = c(rep("A", 100), rep("B", 10), rep("C", 3)),
#'   y = rnorm(113)
#' )
#'
#' # incorrect: bootstrapping ungrouped dataset leads to missing category C
#' ggplot(df, aes(type, y)) +
#'   geom_pointrange(data = bootstrapper(6, seed = 562), stat = "summary") +
#'   facet_wrap(~.draw)
#'
#' # correct: bootstrapping within groups
#' ggplot(df, aes(type, y)) +
#'   geom_pointrange(data = bootstrapper(6, type, seed = 562), stat = "summary") +
#'   facet_wrap(~.draw)
#'
#' # also correct: use grouped data frame
#' ggplot(group_by(df, type), aes(type, y)) +
#'   geom_pointrange(data = bootstrapper(6, seed = 562), stat = "summary") +
#'   facet_wrap(~.draw)
#'
#' \dontrun{
#' library(gganimate)
#'
#' set.seed(69527)
#' x <- rnorm(15)
#' data <- data.frame(
#'   x,
#'   y = x + 0.5*rnorm(15)
#' )
#'
#' bs <- bootstrapper(9)
#'
#' p <- ggplot(data, aes(x, y)) +
#'   geom_point(shape = 21, size = 6, fill = "white") +
#'   geom_text(label = "0", hjust = 0.5, vjust = 0.5, size = 10/.pt) +
#'   geom_point(data = bs, aes(group = .row), shape = 21, size = 6, fill = "blue") +
#'   geom_text(data = bs, aes(label = .copies, group = .row), hjust = 0.5, vjust = 0.5, size = 10/.pt, color = "white") +
#'   geom_smooth(data = bs, method = "lm", se = FALSE) +
#'   ggtitle("Bootstrap demonstration") +
#'   theme_bw()
#'
#' p + facet_wrap(~.draw)
#' p + transition_states(.draw, 1, 1) +
#'   enter_fade() + exit_fade()
#' }
#' @export
bootstrapper <- function(times, group = NULL, seed = NULL, key = ".draw", row = ".row",
                         id = ".id", original_id = ".original_id", copies = ".copies") {
  force(times)
  force(key)
  force(row)
  force(id)
  force(original_id)
  force(copies)
  group <- enquo(group)

  if (is.null(seed)) {
    seed <- sample(2^31-1, 1)
  }

  count_copies <- function(x) {
    z <- as.character(x);
    as.numeric(table(z)[z])
  }

  function(.data) {
    with_seed(
      seed,
      {
        if (!quo_is_null(group)) {
          # if group is given, set up new grouping variable
          boot_group <- eval_tidy(group, data = .data)
          .data <- mutate(.data, .draw_group = boot_group) %>%
            group_by(.draw_group)
        }

        .data %>%
          bootstrapify(times = times, key = key) %>%
          collect(id = id, original_id = original_id) %>%
          mutate(
            !!copies := count_copies(!!as.symbol(original_id)),
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

