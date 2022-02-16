.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "ReSources",
    system.file("dist", package = "ReSources")
  )
}
