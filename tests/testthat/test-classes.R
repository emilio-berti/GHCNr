test_that("daily class", {
  expect_s3_class(CA003076680, "ghcn_daily")
})

test_that("monthly class", {
  expect_s3_class(monthly(CA003076680), "ghcn_monthly")
})

test_that("annual class", {
  expect_s3_class(annual(CA003076680), "ghcn_annual")
})
