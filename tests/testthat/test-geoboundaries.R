test_that("country download", {
  skip_on_cran()
  skip_on_ci()
  cc <- sample(country_codes[["iso3"]], 1)
  roi <- get_country(cc)
  expect_s4_class(roi, "SpatVector")
})

test_that("countries download", {
  skip_on_cran()
  skip_on_ci()
  cc <- sample(country_codes[["iso3"]], 3)
  roi <- get_countries(cc)
  expect_s4_class(roi, "SpatVector")
})
