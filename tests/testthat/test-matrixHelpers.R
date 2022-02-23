context("Test matrix helper functions")

test_that("bindMatrices", {
  x <- matrix(NA, 0, 1, dimnames = list(NULL, "proxy_1"))
  y <- matrix(NA, 1, 1, dimnames = list("Individual_1", "proxy_1"))
  z <- NULL

  m <- bindMatrices(x, y, z)

  expect_equal(m, y)
})


test_that("bindMatrices fiveSources", {
  x <- matrix(NA, 0, 1, dimnames = list(NULL, c("Carbon")))
  y <- matrix(NA, 5, 1, dimnames = list(paste("Individual", 1:5, sep = "_"), "Carbon"))
  z <- NULL

  m <- bindMatrices(x, y, z)

  expect_equal(m, y)
})
