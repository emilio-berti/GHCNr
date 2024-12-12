test_that("monthly coverage", {
  x <- data.frame(
    date = seq(as.Date("1990-01-01"), as.Date("1990-12-31"), by = "day")
  )
  x$month <- as.numeric(format(x$date, "%m"))
  x$tmin <- ifelse(x$month %% 2 == 0, 1, NA)
  x <- as_daily(x)
  expect_equal(
    monthly_coverage(x)[["monthly_coverage_tmin"]],
    rep(c(0, 1), 6)
  )
})

test_that("annual coverage", {
  x <- data.frame(
    date = seq(as.Date("1990-01-01"), as.Date("1991-12-31"), by = "day")
  )
  x$tmin <- rep(c(NA, 1), nrow(x) / 2)
  x$tmax <- rep(c(NA, 1), each = nrow(x) / 2)
  x <- as_daily(x)
  expect_equal(
    annual_coverage(x)[["annual_coverage_tmax"]],
    c(0, 1)
  )
  expect_equal(
    annual_coverage(x)[["annual_coverage_tmin"]],
    c(.5, .5),
    tolerance = 1e-2
  )
})
