context("Set names for covariance matrix")

test_that("same dim", {
    res <- setCovNames(diag(2), c("a", "b"))

    expect_equal(res, matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")) ))
})

test_that("smaller", {
    res <- setCovNames(diag(3), c("a", "b"))

    expect_equal(res, matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")) ))
})

test_that("bigger", {
    res <- setCovNames(diag(2), c("a", "b", "c"))

    exp <- diag(3)
    rownames(exp) <- colnames(exp) <- c("a", "b", "c")
    expect_equal(res, exp)
})

test_that("lists", {
    res <- setCovNames(list(diag(2)), c("a", "b"))
    exp <- matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")) )

    expect_equal(res, list(exp))

})