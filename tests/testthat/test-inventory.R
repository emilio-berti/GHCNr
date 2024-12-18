test_that("correct inventory url", {
  expect_identical(
    "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt",
    .inventory_url()
  )  
})

test_that("reading and filtering inventory", {
  skip_on_cran()
  skip_on_ci()
  vars <- sample(c("tmin", "tmax", "prcp"), sample(1:3, 1))
  first_year <- sample(seq(1960, 2000), 1)
  last_year <- sample(seq(first_year, 2024), 1)
  d <- stations(
    variables = vars,
    first_year = first_year,
    last_year = last_year
  )
  expect_identical(
    sort(unique(d[["variable"]])),
    sort(toupper(vars))
  )
  expect_lte(
    max(d[["startYear"]]),
    first_year
  )
  expect_gte(
    max(d[["endYear"]]),
    last_year
  )
})
