test_that("extract years works", {
  x <- as.POSIXct("1990-3-15", format = "%Y-%m-%d")
  identical(.years(x), "1990")
})
