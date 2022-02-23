#' Check Availability for Mpi Iso App package
#'
#' @export
isoInstalled <- function() {
  "MpiIsoApp" %in% installed.packages()[, 1] &&
    compareVersion(as.character(packageVersion("MpiIsoApp")), isoVersion()) > -1
}

isoVersion <- function() {
  "1.2.5"
}
