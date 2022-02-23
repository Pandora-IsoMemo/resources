context("Check single elements of fruits class")

test_that("checkValueNames", {
  valueNames <- list(
    targets = c("Carbon", "Nitrogen"),
    fractions = c("Protein", "Energy"),
    sources = c("Herbivores", "Carnivores", "Plants", "Fish1", "Fish2")
  )
  # nolint start
  expect_error(ReSources:::checkValueNames(valueNames), NA)
  valueNames$targets <- 1:3
  expect_error(
    ReSources:::checkValueNames(valueNames),
    "valueNames\\$targets has wrong type: integer instead of character"
  )
})
