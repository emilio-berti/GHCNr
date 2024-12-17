test_that("flag warning", {
  d <- CA003076680
  d[["tmax_flag"]][1] <- "I"
  expect_warning(monthly(d))
  expect_warning(annual(d))
  expect_warning(coverage(d))
})

test_that("remove flagged", {
  d <- CA003076680
  flags <- sample(1:nrow(d), sample(100 : (nrow(d) / 2), 1))
  d[["tmax_flag"]][flags] <- "I"
  d <- remove_flagged(d)
  expect_equal(nrow(CA003076680) - length(flags), nrow(d))
})
