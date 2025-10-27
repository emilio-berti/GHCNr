test_that("show missing variables", {
  x <- CA003076680
  remove <- sample(c("tavg", "tmax", "tmin", "prcp"), 1)
  x <- x[, -which(colnames(x) == remove)]
  expect_identical(
    .missing_variables(x),
    remove
  )
})

test_that("add missing variables", {
  x <- CA003076680
  remove <- sample(c("tavg", "tmax", "tmin", "prcp"), 1)
  x <- x[, -which(colnames(x) == remove)]
  x <- .add_variables(x)
  expect_identical(
    sort(colnames(CA003076680)),
    sort(colnames(x))
  )
})

test_that("summary functions", {
  x <- rnorm(100)
  x[sample(seq_along(x), 5)] <- NA
  expect_equal(min(x, na.rm = TRUE), .min(x))
  expect_equal(max(x, na.rm = TRUE), .max(x))
  expect_equal(mean(x, na.rm = TRUE), .mean(x))
  expect_equal(sum(x, na.rm = TRUE), .sum(x))
})
