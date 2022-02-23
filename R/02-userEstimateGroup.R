userEstimateGroupUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 9,
        uiOutput(ns("groups")),
        actionButton(ns("addGroup"), "Add Group")
      ),
      column(
        width = 3,
        tags$br(),
        tableOutput(ns("savedGroups"))
      )
    )
  )
}

userEstimateGroup <- function(input, output, session, userEstimates, groupsInitial) {
  groups <- reactiveVal(list())
  updateUI <- reactiveVal(1)
  observers <- reactiveVal(list())

  observe({
    groups(groupsInitial())
  })

  output$groups <- renderUI({
    userEstimateGroupInput(groups(), userEstimates(), ns = session$ns)
  })

  groupIds <- reactiveVal(NULL)

  observeEvent(input$addGroup, {
    groups(addEmptyGroup(groups()))
    groupIds(sapply(groups(), function(group) group$id))
  })

  observeEvent(groupIds(), {
    lapply(groups(), function(group) {
      if (group$id %in% names(observers())) {
        return()
      }
      # if (!is.null(group$observer) && group$observer) return()
      tagId <- function(x) paste("group", x, group$id, sep = "_")

      observeEvent(input[[tagId("delete")]], {
        groups(deleteGroup(groups(), group$id))
        if (group$id %in% names(observers())) {
          observers(deleteObserver(observers(), group$id))
        }
      })

      observeEvent(input[[tagId("apply")]], {
        if (tagId("name") %in% names(input)) {
          groups(setGroupProperty(isolate(groups()), group$id, "name", input[[tagId("name")]]))
        }
        if (tagId("estimates") %in% names(input)) {
          groups(setGroupProperty(isolate(groups()), group$id, "estimates", input[[tagId("estimates")]]))
        }
        if (tagId("normalize") %in% names(input)) {
          groups(setGroupProperty(isolate(groups()), group$id, "normalize", input[[tagId("normalize")]]))
        }
      })

      # groups(setGroupProperty(groups(), group$id, "observer", TRUE))
      observers(setObserver(observers(), group$id))
    })
  })

  output$savedGroups <- renderTable(
    {
      req(length(groups()) > 0)
      groups() %>%
        bind_rows() %>%
        select(-"id")
    },
    striped = TRUE,
    caption = "saved groups"
  )

  groups
}

userEstimateGroupInput <- function(groups, userEstimates, ns) {
  div(lapply(groups, userEstimateGroupInputRow, estimates = userEstimates, ns = ns))
}

userEstimateGroupInputRow <- function(group, estimates, ns = ns) {
  tagId <- function(x) ns(paste("group", x, group$id, sep = "_"))

  div(
    class = "user-estimate-group",
    textInput(tagId("name"), "Group", value = group$name),
    pickerInput(tagId("estimates"), "Estimates",
      choices = estimates,
      selected = group$estimates, multiple = TRUE
    ),
    checkboxInput(tagId("normalize"), "Normalize", value = group$normalize),
    actionButton(tagId("apply"), "Apply"),
    actionButton(tagId("delete"), "Remove")
  )
}

extractGroupIds <- function(groups) {
  unlist(lapply(groups, `[[`, "id"))
}

getGroupIndex <- function(groups, id) {
  which(extractGroupIds(groups) == id)
}

addEmptyGroup <- function(groups) {
  n <- length(groups) + 1

  id <- paste(sample(letters, 12, replace = TRUE), collapse = "")

  groups[[n]] <- list(
    id = id,
    name = "",
    estimates = NULL,
    normalize = FALSE
  )

  groups
}

deleteGroup <- function(groups, id) {
  i <- getGroupIndex(groups, id)

  if (length(i) == 1) {
    groups[[i]] <- NULL
  }

  groups
}

setGroupProperty <- function(groups, id, key, value) {
  i <- getGroupIndex(groups, id)

  if (length(i) == 0) {
    return(groups)
  }

  groups[[i]][[key]] <- value

  groups
}

setObserver <- function(observers, groupId) {
  observers[[groupId]] <- TRUE
  observers
}

deleteObserver <- function(observers, groupId) {
  observers[[groupId]] <- NULL
  observers
}
