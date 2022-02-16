context("Load data")

test_that("duplicate rownames", {
  res <- loadData("sources.xlsx", "xlsx", rownames = TRUE)

  expect_equal(dim(res), c(48, 6))
  expect_setequal(rownames(res), c("Plants", "TerrestrialAnimals", "MarineFish", "FreshwaterFish"))
})
