context("Handle double matrices")

test_that("Split", {
  m <- cbind(diag(2), diag(2))
  rownames(m) <- 1:2
  colnames(m) <- c("Var1||x", "Var1||y", "Var2||x", "Var2||y")

  split <- splitDoubleMatrix(m)

  res1 <- matrix(c(1, 0, 1, 0), 2, 2)
  rownames(res1) <- 1:2
  colnames(res1) <- c("Var1", "Var2")

  res2 <- matrix(c(0, 1, 0, 1), 2, 2)
  rownames(res2) <- 1:2
  colnames(res2) <- c("Var1", "Var2")

  expect_equal(split[[1]], res1)
  expect_equal(split[[2]], res2)
})

test_that("Remove empty columns", {
  m <- cbind(diag(2), NA, NA)
  rownames(m) <- 1:2
  colnames(m) <- c("Var1||x", "Var1||y", "", "")

  split <- splitDoubleMatrix(m)

  res1 <- matrix(c(1, 0), 2, 1)
  res2 <- matrix(c(0, 1), 2, 1)
  rownames(res1) <- 1:2
  colnames(res1) <- "Var1"
  rownames(res2) <- 1:2
  colnames(res2) <- "Var1"

  expect_equal(split[[1]], res1)
  expect_equal(split[[2]], res2)
})

test_that("Combine matrices", {
  var1 <- diag(2)
  var2 <- diag(2)

  colnames(var1) <- c("A", "B")
  rownames(var1) <- 1:2

  m <- combineDoubleMatrix(var1, var2)

  res <- matrix(c(1, 0, 1, 0, 0, 1, 0, 1), 2, 4)
  colnames(res) <- c("A||mean", "A||uncert", "B||mean", "B||uncert")
  rownames(res) <- 1:2

  expect_equal(m, res)
})
