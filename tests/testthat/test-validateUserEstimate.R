context("Validate user estimate input")

test_that("Invalid input", {
  expect_error(validateUserEstimate(3))
})

test_that("Additional = symbol", {
  expect_false(validateUserEstimate("bla=[test]=[test]+2"))
  expect_false(validateUserEstimate("blupp=[test]=[test]=2"))
})

test_that("Operator missing", {
  expect_false(validateUserEstimate("est1=[test][test]"))
  expect_false(validateUserEstimate("est2=[test]+[test][test]"))
  expect_false(validateUserEstimate("est3=[test]-123[test]"))
  expect_false(validateUserEstimate("est4=[test]*[test]321"))
})

test_that("Too many operators", {
  expect_false(validateUserEstimate("est1=[test]++[test]"))
  expect_false(validateUserEstimate("est2=[test]*-3"))
  expect_false(validateUserEstimate("est3=[test]*/3"))
})

test_that("Valid user estimate", {
  expect_true(validateUserEstimate("est1=[test]+3*2"))
  expect_true(validateUserEstimate("est2=3+[test]*[that]"))
})

test_that("Name already taken", {
  expect_false(validateUserEstimate("test=4", c("test=[this]+3", "test2=3+[that]")))
})

test_that("Invalid name", {
  expect_false(validateUserEstimate("test 1="))
  expect_false(validateUserEstimate("test*1="))
  expect_false(validateUserEstimate("test-1="))
  expect_false(validateUserEstimate("test+1="))
  expect_false(validateUserEstimate("test/1="))
  expect_false(validateUserEstimate("test(1="))
  expect_false(validateUserEstimate("test)1="))
  expect_false(validateUserEstimate("test{1="))
  expect_false(validateUserEstimate("test}1="))
  expect_false(validateUserEstimate("test[1="))
  expect_false(validateUserEstimate("test]1="))
})

test_that("Valid name", {
  expect_true(validateUserEstimate("test=3", c("test2=[this]+3", "test3=3+[that]")))
  expect_true(validateUserEstimate("test", NULL))
})
