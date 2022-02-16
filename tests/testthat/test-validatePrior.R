context("Validate prior input")

test_that("Invalid input", {
  expect_error(validatePrior(c("a", "b")))
  expect_error(validatePrior(3))
})

test_that("Prior (LHS / RHS) missing", {
  expect_false(validatePrior(""))
  expect_false(validatePrior(">="))
  expect_false(validatePrior(">[test]"))
  expect_false(validatePrior("[test]>="))
})

test_that("Operator missing", {
  expect_false(validatePrior("[test]"))
  expect_false(validatePrior("[test][test]"))
  expect_false(validatePrior("[test]>[test][test]"))
  expect_false(validatePrior("[test]>123[test]"))
  expect_false(validatePrior("[test]>[test]321"))
})

test_that("Too many operators", {
  expect_false(validatePrior("[test]>[test]>3"))
  expect_false(validatePrior("[test]<=[test]<2"))
  expect_false(validatePrior("[test]>[test]>=[test]"))
  expect_false(validatePrior("[test]++3>[test]"))
  expect_false(validatePrior("[test]//3>[test]"))
})

test_that("Correct priors", {
  expect_true(validatePrior("[test]>[test]"))
  expect_true(validatePrior("[test]>=[test]+[test]"))
  expect_true(validatePrior("[test]-3<[test]"))
})
