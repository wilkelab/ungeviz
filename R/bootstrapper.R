#' Create a bootstrapper function useful for repeated reproducible bootstrapping
#'
#' @param ndraws Number of independent bootstrap draws to perform.
#' @param ... Variables to group by.
#' @param .seed Random seed to use.
#' @param .draw_key Name of the column that will hold an integer
#'   running from 1 to `ndraws` indicating the bootstrap replicate.
#' @param .row_key Name of the column that will hold an integer
#'   counting rows in the final bootstrapped dataset. Useful for
#'   animations with gganimate.
#' @export
bootstrapper <- function(ndraws, ..., .seed = NULL, .draw_key = ".draw",
                         .row_key = ".row") {
  args <- enquos(...)

  if (is.null(.seed)) {
    .seed <- sample(2^31-1, 1)
  }

  function(.data) {
    with_seed(
      .seed,
      {
        .data %>%
          group_by(!!!args) %>%
          bootstrap(ndraws) %>%
          mutate(
            !!.row_key := 1
          ) -> out
        # need to set row numbers outside of `mutate()`
        # to circumvent grouping
        out[.row_key] <- 1:nrow(out)
        out
      }
    )
  }
}
