context("Import Datafile")

df <- data.frame(
  num = c(0, 1, 2, 3, pi),
  char = letters[1:5],
  stringsAsFactors = FALSE
)

test_that("csv", {
  file <- paste0(tempdir(), "test.csv")
  write.csv(df, file, row.names = FALSE)

  expect_equal(loadData(file, "csv"), df)
  expect_error(loadData(file, "xlsx"))
})

test_that("xlsx", {
  file <- paste0(tempdir(), "test.xlsx")
  write.xlsx(df, file)

  expect_equal(loadData(file, "xlsx"), df)
})
