context("Extend nested lists")

test_that("Simple list", {
  what <- list(
    a = 1,
    b = 2
  )

  with1 <- list(
    c = 3,
    d = 4
  )

  with2 <- list(
    a = 3
  )

  expect_equal(
    extend(what, with1),
    list(a = 1, b = 2, c = 3, d = 4)
  )
  expect_equal(
    extend(what, with2),
    list(a = 1, b = 2)
  )
  expect_equal(
    extend(what, with2, overwrite = TRUE),
    list(a = 3, b = 2)
  )
  expect_equal(
    extend(what, with2, strip = TRUE),
    list(a = 1)
  )
  expect_equal(
    extend(what, with2, strip = TRUE, overwrite = TRUE),
    list(a = 3)
  )
})

test_that("Nested list", {
  what <- list(
    a = list(),
    b = list(C = 7, D = list())
  )

  with1 <- list(
    a = list(
      A = 1,
      B = 2
    ),
    b = list(
      C = 3,
      D = list(x = 1, y = 2)
    )
  )

  expect_equal(
    extend(what, with1),
    list(
      a = list(
        A = 1,
        B = 2
      ),
      b = list(
        C = 7,
        D = list(x = 1, y = 2)
      )
    )
  )

  expect_equal(
    extend(what, with1, overwrite = TRUE),
    list(
      a = list(
        A = 1,
        B = 2
      ),
      b = list(
        C = 3,
        D = list(x = 1, y = 2)
      )
    )
  )
})


test_that("Empty matrix", {
  what <- list(
    a = matrix(NA, 0, 0)
  )

  with <- list(
    a = 1
  )

  expect_equal(
    extend(what, with),
    list(a = 1)
  )
})

test_that("Numeric indices", {
  what <- list("a")
  with <- list("c", "d")

  expect_equal(extend(what, with), list("a", "d"))
  expect_equal(extend(what, with, strip = TRUE), list("a", "d"))
  expect_equal(extend(with, what, strip = TRUE), list("c"))
})

test_that("Numeric Indices 2", {
  what <- list(
    list(
      "default" = matrix(2, 1, 1)
    )
  )

  with <- list(
    list(
      "default" = matrix(NA, 1, 1)
    )
  )

  expect_equal(extend(what, with), what)
  expect_equal(extend(what, with, strip = TRUE), what)
})

test_that("Edge case", {
  dummy <- ll <- list(
    list(
      a = matrix(NA, 5, 2),
      b = matrix(NA, 5, 2)
    )
  )

  ll[[1]]$a[1, 1] <- 4

  expect_equal(extend(ll, dummy), ll)
  expect_equal(extend(ll, dummy, strip = TRUE), ll)
})
