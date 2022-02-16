context("Set default names")

test_that("set default names", {
  expect_equal(defaultNames(character(0)), character(0))
  expect_equal(defaultNames("", prefix = "test"), "test_1")
  expect_equal(defaultNames(c("", "test"), prefix = "test"), c("test_1", "test"))
})

test_that("Repeated Values", {
  expect_equal(defaultNames(c("test", "test", ""), prefix = "test"), c("test", "test", "test_3"))
})

test_that("Values exist", {
  expect_equal(defaultNames(c("", "test_1"), prefix = "test"), c("test_1a", "test_1"))
  expect_equal(defaultNames(c("", "test_1", "test_2"), prefix = "test"),
               c("test_1a", "test_1", "test_2"))
  expect_equal(defaultNames(c("", "test_1", "test_1a"), prefix = "test"),
               c("test_1b", "test_1", "test_1a"))

})
