fileNotesDialog <- function(id, value){
  modalDialog(
    textAreaInput(id, "File Notes", value, width = "100%", height = "400px") %>%
      tagAppendAttributes(style = 'width: 100%;'), footer = modalButton("Ok")
  )
}
