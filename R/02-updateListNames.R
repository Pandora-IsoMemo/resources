#' Update Targets In Lists
#'
#' Update all list entries named "name" from all lists that contain entries named by targets.
#'
#' @param values (list) list containing all input data (all input tables)
#' @param name (character) name of the target to be removed (only )
#' @param updateFun name of function that updates the lists, either
#'  deleteTableFromList or updateListNames
updateTargetsInLists <- function(values, name, updateFun) {
  for (entry in c("source",
                  "sourceUncert",
                  "sourceOffset",
                  "sourceOffsetUncert")) {
    # use isDeepestEntry()
    depth <-
      getDepthAndTable(values[[entry]], isEntryFun = isDeepestEntry)$nFlatten
    
    values[[entry]] <- updateFun(entryContent = values[[entry]],
                                 depth = depth,
                                 namesList = name)
  }
  
  values
}


#' Remove Obsvn From Lists
#'
#' Removes all list entries named "name" from all lists that contain entries named by observations.
#'
#' @param values (list) list containing all input data (all input tables)
#' @param name (character) name of the target to be removed
removeObsvnFromLists <- function(values, name) {
  for (entry in c("source",
                  "sourceUncert",
                  "sourceOffset",
                  "sourceOffsetUncert",
                  "sourceCovariance",
                  "concentration",
                  "concentrationUncert",
                  "concentrationCovariance")) {
    values[[entry]] <- removeObsvnFromEntry(values, entry, name)
  }
  
  values
}


#' Remove Obsvn From Entry
#'
#' Removes all list entries named "name" from the element "entry".
#'
#' @param values (list) list containing all input data (all input tables)
#' @param entry (character) one of c("source", "sourceUncert", "sourceOffset", "sourceOffsetUncert",
#'  "sourceCovariance", "concentration", "concentrationUncert", "concentrationCovariance")
#' @param name (character) name of the target to be removed
removeObsvnFromEntry <- function(values, entry, name) {
  if (entry %in% c("source",
                   "sourceUncert",
                   "sourceOffset",
                   "sourceOffsetUncert")) {
    isEntryFun <- isPreDeepestEntry
  } else if (entry %in% c("sourceCovariance",
                          "concentration",
                          "concentrationUncert",
                          "concentrationCovariance")) {
    isEntryFun <- isDeepestEntry
  } else {
    warning("No method to delete obsvn from this entry.")
    return(values[[entry]])
  }
    
  depth <-
    getDepthAndTable(values[[entry]], isEntryFun = isEntryFun)$nFlatten
  
  deleteTableFromList(values[[entry]],
                      depth = depth,
                      namesList = name)
}


#' Delete Table From List
#'
#' @param entryContent (list) possibly nested list
#' @param depth depth of list where names should be updated
#' @param namesList names of list elements to be deleted
deleteTableFromList <- function(entryContent, depth, namesList) {
  if (depth == 0) {
    if (!is.null(names(entryContent))) {
      entryContent[!(names(entryContent) %in% namesList)]
    } else {
      warning("Tried to delete named entry from unnamed list. Please debug!")
      entryContent
    }
    
  } else {
    depth <- depth - 1
    lapply(entryContent, function(elem) {
      deleteTableFromList(elem, depth, namesList)
    })
  }
}


#' Update List Names
#' 
#' @param entryContent (list) possibly nested list
#' @param depth depth of list where names should be updated
#' @param namesList new names for list elements
updateListNames <- function(entryContent, depth, namesList) {
  if (depth == 0) {
    names(entryContent) <- namesList[1:length(names(entryContent))]
    entryContent
  } else {
    depth <- depth - 1
    lapply(entryContent, function(elem) {
      updateListNames(elem, depth, namesList)
    })
  }
}


#' Get Depth And Table
#'
#' Get the list depth and the content of the table.
#'
#' @param entryContent (list) list to look for names
#' @param isEntryFun (function) function that checks for the correct level in the list hierarchy
#' @param n (numeric) depth of list to look for names
#' of values
getDepthAndTable <-
  function(entryContent,
           isEntryFun = isDeepestEntry,
           n = NULL) {
    nFlatten <- 0
    if (is.null(n)) {
      while (!isEntryFun(entryContent)) {
        # go one level deeper to compare names:
        entryContent <- entryContent[[1]]
        nFlatten <- nFlatten + 1
      }
    } else {
      while (nFlatten < n) {
        # go one level deeper to compare names:
        entryContent <- entryContent[[1]]
        nFlatten <- nFlatten + 1
      }
    }
    
    list(nFlatten = nFlatten,
         entryContent = entryContent)
  }


#' Is Deepest Entry
#' 
#' Checks if names of the list are targetNames (deepest hierarchy in a values object)
#' 
#' @param entryContent (list) element of values, e.g. values$source, values$sourceUncert,
#'  values$sourceOffset, values$sourceOffsetUncert
isDeepestEntry <- function(entryContent) {
  !is.null(ncol(entryContent[[1]])) && 
    is.null(names(entryContent[[1]][[1]]))
}


#' Is Pre Deepest Entry
#' 
#' Checks if names if the list are obsvnNames (hierarchy above targetNames in a values object)
#' 
#' @param entryContent (list) element of values, e.g. values$source, values$sourceUncert,
#'  values$sourceOffset, values$sourceOffsetUncert
isPreDeepestEntry <- function(entryContent) {
  !is.null(ncol(entryContent[[1]][[1]])) && 
    is.null(names(entryContent[[1]][[1]][[1]]))
}
