# run with source("RScripts/shinytest.R")

# Update functions called in app
devtools::install()

library(shinytest)

recordTest("./inst/app")

# Confirm testfile in emacs with C+x #
