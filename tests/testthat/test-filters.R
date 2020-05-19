test_that("meanFilter returns the mean", {
  expect_equal(meanFilter(c(1, 2, 3, 4, 5, 6, 7)), c(NA, NA, 3, 4, 5, NA, NA))
  expect_equal(meanFilter(c(1, 1, 1, 1, 1, 1, 1)), c(NA, NA, 1, 1, 1, NA, NA))
  expect_equal(meanFilter(c(1, 1, 1, 1, 1, 1, 1), n = 2), c(NA, 1, 1, 1, 1, 1, NA))
})

test_that("meanFilter attaches properly to data.table")
