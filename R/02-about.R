aboutDialog <- function() {
  modalDialog(includeHTML(
    system.file("htmlTemplate", "about.html", package = "ReSources")
  ), footer = modalButton("Ok"))
}
