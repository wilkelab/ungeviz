context("test-hpline-vpline")

test_that("hpline visual test", {
  df <- data.frame(x = c("A", "B", "C"), y = c(2, 1, 3))
  p <- ggplot(df, aes(x, y)) + geom_hpline()

  vdiffr::expect_doppelganger("hpline basic", p)
  vdiffr::expect_doppelganger("hpline color mapping", p + aes(color = x))
  vdiffr::expect_doppelganger("hpline size mapping", p + aes(size = y))

  p <- ggplot(df, aes(x, y)) + geom_hpline(width = 1)
  vdiffr::expect_doppelganger("hpline width = 1", p)
})

test_that("vpline visual test", {
  df <- data.frame(y = c("A", "B", "C"), x = c(2, 1, 3))
  p <- ggplot(df, aes(x, y)) + geom_vpline()

  vdiffr::expect_doppelganger("vpline basic", p)
  vdiffr::expect_doppelganger("vpline color mapping", p + aes(color = y))
  vdiffr::expect_doppelganger("vpline size mapping", p + aes(size = x))

  p <- ggplot(df, aes(x, y)) + geom_vpline(height = 1)
  vdiffr::expect_doppelganger("vpline width = 1", p)
})
