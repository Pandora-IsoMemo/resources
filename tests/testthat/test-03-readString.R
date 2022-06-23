context("Paste shiny matrix")

test_that("readString/Wrapper numeric matrix", {
  testContent <-
    "\tOffset||mean\tOffset||uncert\nD13C\t4.8\t0.5\nD15N\t5.5\t0.5\nD13Capa\t10.1\t0.5"
  testMode <- "auto"
  testClass <- "numeric"
  
  res1 <- readString(content = testContent, mode = testMode)
  res2 <-
    readStringWrapper(content = testContent,
                      mode = testMode,
                      class = testClass)
  
  exp1 <-
    structure(
      c(
        "D13C",
        "D15N",
        "D13Capa",
        " 4.8",
        " 5.5",
        "10.1",
        "0.5",
        "0.5",
        "0.5"
      ),
      dim = c(3L, 3L),
      dimnames = list(NULL,
                      c("", "Offset||mean", "Offset||uncert"))
    )
  exp2 <-
    structure(c(4.8, 5.5, 10.1, 0.5, 0.5, 0.5),
              dim = 3:2,
              dimnames = list(
                c("D13C", "D15N", "D13Capa"),
                c("Offset||mean", "Offset||uncert")
              ))
  
  testthat::expect_equal(res1, exp1)
  testthat::expect_equal(res2, exp2)
})




test_that("readString/Wrapper character matrix", {
  testContent <-
    "\tcovariate\nCluster1\tCluster_1\nCluster2\tCluster_2"
  testMode <- "auto"
  testClass <- "character"
  
  res1 <- readString(content = testContent, mode = testMode)
  res2 <-
    readStringWrapper(content = testContent,
                      mode = testMode,
                      class = testClass)
  
  exp1 <-
    structure(
      c("Cluster1", "Cluster2", "Cluster_1", "Cluster_2"),
      dim = c(2L,
              2L),
      dimnames = list(NULL, c("", "covariate"))
    )
  exp2 <-
    structure(
      c("Cluster_1", "Cluster_2"),
      dim = 2:1,
      dimnames = list(c("Cluster1", "Cluster2"), "covariate")
    )
  
  testthat::expect_equal(res1, exp1)
  testthat::expect_equal(res2, exp2)
})


test_that("readString covariance matrix", {
  testContent <-
    "\tD13C\tD15N\tD13Capa\nD13C\t0.5532011\t-0.3272693\t-0.1832409\nD15N\t-0.3272693\t0.3210752\t0.1772385\nD13Capa\t-0.1832409\t0.1772385\t0.801186"
  testMode <- "auto"
  testClass <- "numeric"
  
  res1 <- readString(content = testContent, mode = testMode)
  res2 <-
    readStringWrapper(content = testContent,
                      mode = testMode,
                      class = testClass)
  
  exp1 <-
    structure(
      c(
        "D13C",
        "D15N",
        "D13Capa",
        " 0.5532011",
        "-0.3272693",
        "-0.1832409",
        "-0.3272693",
        " 0.3210752",
        " 0.1772385",
        "-0.1832409",
        " 0.1772385",
        " 0.8011860"
      ),
      dim = 3:4,
      dimnames = list(NULL,
                      c("", "D13C", "D15N", "D13Capa"))
    )
  exp2 <-
    structure(
      c(
        0.5532011,
        -0.3272693,
        -0.1832409,
        -0.3272693,
        0.3210752,
        0.1772385,
        -0.1832409,
        0.1772385,
        0.801186
      ),
      dim = c(3L, 3L),
      dimnames = list(c("D13C", "D15N", "D13Capa"), c("D13C", "D15N", "D13Capa"))
    )
  
  testthat::expect_equal(res1, exp1)
  testthat::expect_equal(res2, exp2)
})
