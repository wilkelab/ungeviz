context("test-sampler")

test_that("sampling works and is repeatable", {
  df <- data.frame(letter = letters[1:4], number = 1:4)

  spl <- sampler(5, 3)
  expect_equal(spl(df), spl(df))

  x <- spl(df)
  expect_equal(names(table(x$.draw)), c("1", "2", "3", "4", "5"))
  expect_equal(unname(as.numeric(table(x$.draw))), rep(3, 5))
  expect_equal(names(table(x$.id)), c("1", "2", "3"))
  expect_equal(unname(as.numeric(table(x$.id))), rep(5, 3))
  expect_equal(x$number, x$.original_id)
})

test_that("sampling with replacement", {
  df <- data.frame(letter = letters[1:4], number = 1:4)

  spl <- sampler(1, 10, replace = TRUE)
  expect_equal(spl(df), spl(df))

  x <- spl(df)
  expect_equal(x$.draw, rep(1L, 10))
  expect_equal(x$number, x$.original_id)
})

test_that("grouped sampling", {
  df <- data.frame(type = rep(c("A", "B"), each = 2), letter = letters[1:4], number = 1:4)

  spl <- sampler(2, 5, replace = TRUE, seed = 123)
  spl_grp <- sampler(2, 5, group = type, replace = TRUE, seed = 123)

  x1 <- spl(dplyr::group_by(df, type))
  x2 <- spl_grp(df)
  expect_equal(x1$letter, x2$letter)
  expect_equal(names(table(x1$.draw)), c("1", "2"))
  expect_equal(unname(as.numeric(table(x1$.draw))), c(10, 10))
})
