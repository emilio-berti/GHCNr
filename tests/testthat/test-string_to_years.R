test_that("extract years works", {
  x <- as.POSIXct("1990-3-15", format = "%Y-%m-%d")
  identical(format(x, "%Y"), "1990")
})
