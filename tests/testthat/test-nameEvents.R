context("Automatically update matrix names")

test_that("Create events from changes (insert)", {
  # add single name at the end
  old <- matrix(NA, 2, 2)
  new <- matrix(NA, 3, 2)

  colnames(old) <- colnames(new) <- c("name-1", "name-2")

  rownames(old) <- c("name-1", "name-2")
  rownames(new) <- c("name-1", "name-2", "name-3")

  expect_equal(createNameEvents(old, old, row = "a", col = "b"), list())

  events <- createNameEvents(old, new, row = "a", col = "b")
  expect_equal(events, list(
    list(event = "insert", variable = "a", old = NULL, new = "name-3")
  ))
  e2 <- list(
    list(event = "insert", variable = "a", old = NULL, new = "name-3")
  )
  events <- createNameEvents(old, t(new), row = "a", col = "b")
  expect_equal(events, list(
    list(event = "insert", variable = "b", old = NULL, new = "name-3")
  ))

  # add multiple names
  new <- matrix(NA, 4, 2)
  colnames(new) <- colnames(old)
  rownames(new) <- c("name-1", "name-2", "name-3", "name-4")
  events <- createNameEvents(old, new, row = "a", col = "b")
  expect_equal(events, list(
    list(event = "insert", variable = "a", old = NULL, new = "name-3"),
    list(event = "insert", variable = "a", old = NULL, new = "name-4")
  ))

  # add names in row and column
  new <- matrix(NA, 3, 3)
  colnames(new) <- rownames(new) <- paste0("name-", 1:3)

  events <- createNameEvents(old, new, row = "a", col = "b")
  expect_equal(events, list(
    list(event = "insert", variable = "a", old = NULL, new = "name-3"),
    list(event = "insert", variable = "b", old = NULL, new = "name-3")
  ))
})

test_that("Create events from changes (update)", {
  old <- new <- matrix(NA, 2, 2)

  colnames(old) <- paste0("name-", 1:2)
  colnames(new) <- c("name-1", "newname-2")

  rownames(old) <- paste0("rowname-", 1:2)
  rownames(new) <- c("rowname-1", "newrowname-2")

  events <- createNameEvents(old, new, row = "a", col = "b")
  expect_equal(events, list(
    list(event = "update", variable = "a", old = "rowname-2", new = "newrowname-2"),
    list(event = "update", variable = "b", old = "name-2", new = "newname-2")
  ))
})

test_that("Create (no) events for repeated Measures", {
  # insert
  old <- matrix(NA, 2, 2)
  new <- matrix(NA, 3, 3)

  colnames(old) <- paste("name", c(1, 2))
  rownames(old) <- paste("rowname", c(1, 2))

  colnames(new) <- paste("name", c(1, 2, 2))
  rownames(new) <- paste("rowname", c(1, 1, 2))

  expect_equal(createNameEvents(old, new, row = "a", col = "b"), list())

  # update
  old <- matrix(NA, 3, 2)
  new <- matrix(NA, 3, 2)

  colnames(old) <- colnames(new) <- paste("name", c(1, 2))

  rownames(old) <- paste("rowname", c(1, 1, 2))
  rownames(new) <- paste("rowname", c(1, 2, 2))

  expect_equal(createNameEvents(old, new, row = "a", col = "b"), list())

  rownames(new) <- paste("rowname", c(1, 3, 2))
  expect_equal(
    createNameEvents(old, new, row = "a", col = "b"),
    list(
      list(event = "insert", variable = "a", old = NULL, new = "rowname 3")
    )
  )

  # remove
  old <- matrix(NA, 3, 3)
  new <- matrix(NA, 2, 2)

  colnames(old) <- paste("name", c(1, 2, 2))
  rownames(old) <- paste("rowname", c(1, 1, 2))

  colnames(new) <- paste("name", c(1, 2))
  rownames(new) <- paste("rowname", c(1, 2))

  expect_equal(createNameEvents(old, new, row = "a", col = "b"), list())
})

test_that("Process insert event", {
  events <- list(
    list(event = "insert", variable = "a", old = NULL, new = "new-name")
  )

  m <- matrix(NA, 1, 1)
  colnames(m) <- c("name-1")
  rownames(m) <- c("rowname-1")

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), {
    res <- matrix(NA, 2, 1)
    colnames(res) <- colnames(m)
    rownames(res) <- c("rowname-1", "new-name")
    res
  })

  expect_equal(processNameEvents(m, events, row = "b", col = "a"), {
    res <- matrix(NA, 1, 2)
    rownames(res) <- rownames(m)
    colnames(res) <- c("name-1", "new-name")
    res
  })
})

test_that("Process update event", {
  events <- list(
    list(event = "update", variable = "a", old = "name-1", new = "new-name")
  )

  m <- matrix(NA, 1, 1)
  colnames(m) <- c("name-1")
  rownames(m) <- c("name-1")

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), {
    res <- m
    rownames(res) <- c("new-name")
    res
  })

  expect_equal(processNameEvents(m, events, row = "b", col = "a"), {
    res <- m
    colnames(res) <- c("new-name")
    res
  })
})

test_that("Process remove event", {
  events <- list(
    list(event = "remove", variable = "a", old = "name-1", new = NULL)
  )

  m <- matrix(1, 2, 1)
  colnames(m) <- c("name-1")
  rownames(m) <- c("name-1", "name-2")

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), {
    res <- matrix(1, 1, 1)
    rownames(res) <- c("name-2")
    colnames(res) <- c("name-1")
    res
  })

  expect_equal(processNameEvents(m, events, row = "b", col = "a"), {
    res <- matrix(1, 2, 0)
    rownames(res) <- c("name-1", "name-2")
    res
  })
})

test_that("Process (no) events for repeated measures", {
  # insert
  events <- list(
    list(event = "insert", variable = "a", old = "name-1", new = "new-name")
  )

  m <- matrix(NA, 2, 2)
  colnames(m) <- c("name-1", "new-name")
  rownames(m) <- c("name-1", "new-name")

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), m)
  expect_equal(processNameEvents(m, events, row = "b", col = "a"), m)

  # update
  events <- list(
    list(event = "update", variable = "a", old = "old-name", new = "new-name")
  )

  m <- matrix(NA, 2, 2)
  colnames(m) <- c("old-name", "old-name")
  rownames(m) <- c("old-name", "old-name")

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), {
    res <- m
    rownames(res) <- c("new-name", "new-name")
    res
  })

  expect_equal(processNameEvents(m, events, row = "b", col = "a"), {
    res <- m
    colnames(res) <- c("new-name", "new-name")
    res
  })

  # remove event
  events <- list(
    list(event = "remove", variable = "a", old = "name-1", new = NULL)
  )

  m <- matrix(1, 2, 2)
  rownames(m) <- rep("name-1", 2)
  colnames(m) <- rep("name-1", 2)

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), {
    res <- matrix(1, 0, 2)
    colnames(res) <- colnames(m)
    res
  })

  expect_equal(processNameEvents(m, events, row = "b", col = "a"), {
    res <- matrix(1, 2, 0)
    rownames(res) <- rownames(m)
    res
  })
})

test_that("Process update events first", {
  m <- matrix(NA, 1, 1)
  colnames(m) <- "col"
  rownames(m) <- "A"

  events <- list(
    list(event = "insert", variable = "a", old = NULL, new = "B"),
    list(event = "insert", variable = "a", old = NULL, new = "C"),
    list(event = "update", variable = "a", old = "A", new = "B")
  )

  expect_equal(processNameEvents(m, events, row = "a", col = "b"), {
    res <- matrix(NA, 2, 1)
    rownames(res) <- c("B", "C")
    colnames(res) <- "col"
    res
  })
})
