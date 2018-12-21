#' Create a bootstrapper function useful for repeated reproducible bootstrapping
#'
#' @param times Number of independent bootstrap draws to perform.
#' @param group An optional expression setting up the grouping to use for bootstrapping.
#'   If not provided, any grouping present in the original dataset will be used.
#' @param seed Random seed to use.
#' @param key Name of the column that will hold an integer
#'   running from 1 to `times` indicating the bootstrap replicate.
#' @param row Name of the column that will hold an integer
#'   counting rows in the final bootstrapped dataset. Useful for
#'   animations with gganimate.
#' @param id Name of the column that will hold an integer
#'   running from 1 to n for each bootstrap, where n is the number
#'   of observations in each group
#' @param original_id Name of the column that indicate the
#'   original row that the bootstrapped row came from
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   geom_smooth(data = bootstrapper(5, Species), aes(group = .bootstrap), se = FALSE) +
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
#'   facet_wrap(~.bootstrap)
#'
#' # correct: bootstrapping within groups
#' ggplot(df, aes(type, y)) +
#'   geom_pointrange(data = bootstrapper(6, type, seed = 562), stat = "summary") +
#'   facet_wrap(~.bootstrap)
#'
#' # also correct: use grouped data frame
#' ggplot(group_by(df, type), aes(type, y)) +
#'   geom_pointrange(data = bootstrapper(6, seed = 562), stat = "summary") +
#'   facet_wrap(~.bootstrap)
#' @export
bootstrapper <- function(times, group = NULL, seed = NULL, key = ".bootstrap",
                         row = ".row", id = ".id", original_id = ".original_id") {
  force(times)
  group <- enquo(group)
  #key <- enquo(key)
  #row <- enquo(row)
  #id <- enquo(id)
  #original_id <- enquo(original_id)

  if (is.null(seed)) {
    seed <- sample(2^31-1, 1)
  }

  function(.data) {
    with_seed(
      seed,
      {
        if (!quo_is_null(group)) {
          # if group is given, set up new grouping variable
          boot_group <- eval_tidy(group, data = .data)
          .data <- mutate(.data, .bootstrap_group = boot_group) %>%
            group_by(.bootstrap_group)
        }

        .data %>%
          bootstrapify(times, key = !!key) %>%
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

