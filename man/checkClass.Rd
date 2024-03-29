% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-fruitsClass.R
\name{checkClass}
\alias{checkClass}
\title{Check if class is in a set of accepted classes}
\usage{
checkClass(x, classesExpected = "list", argName = NULL)
}
\arguments{
\item{x}{any object}

\item{classesExpected}{character: vector with accepted classes}

\item{argName}{character: name of argument displayed in error message}
}
\description{
Check if class is in a set of accepted classes
}
\examples{
ReSources:::checkClass(c(1, 2, 3), c("numeric", "integer"))
\dontrun{
ReSources:::checkClass("This is not a list", argName = "x")
}
}
