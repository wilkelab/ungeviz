context("test-bootstrapper")

test_that("bootstrapping works and is repeatable", {
  df <- data.frame(letter = letters[1:4], number = 1:4)

  bsr <- bootstrapper(5)
  expect_equal(bsr(df), bsr(df))

  x <- bsr(df)
  expect_equal(names(table(x$.draw)), c("1", "2", "3", "4", "5"))
  expect_equal(unname(as.numeric(table(x$.draw))), rep(4, 5))
  expect_equal(names(table(x$.id)), c("1", "2", "3", "4"))
  expect_equal(unname(as.numeric(table(x$.id))), rep(5, 4))
  expect_equal(x$number, x$.original_id)
})

test_that("grouped bootstrapping", {
  df <- data.frame(type = rep(c("A", "B"), each = 2), letter = letters[1:4], number = 1:4)

  bsr <- bootstrapper(2, seed = 123)
  bsr_grp <- bootstrapper(2, group = type, seed = 123)

  x1 <- bsr(dplyr::group_by(df, type))
  x2 <- bsr_grp(df)
  expect_equal(x1$letter, x2$letter)
  expect_equal(names(table(x1$.draw)), c("1", "2"))
  expect_equal(unname(as.numeric(table(x1$.draw))), c(4, 4))
})

