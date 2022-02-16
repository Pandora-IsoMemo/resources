#' Start Application
#'
#' @param port port of web application
#' @param host accept connections from this address
#'
#' @export
startApplication <- function(port = 4242, host = "127.0.0.1") {
  runApp(
    system.file("app", package = "ReSources"),
    port = port,
    host = host
  )
}
