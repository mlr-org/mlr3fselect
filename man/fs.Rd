% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sugar.R
\name{fs}
\alias{fs}
\alias{fss}
\title{Syntactic Sugar for Feature Selection Objects Construction}
\usage{
fs(.key, ...)

fss(.keys, ...)
}
\arguments{
\item{.key}{(\code{character(1)})\cr
Key passed to the respective \link[mlr3misc:Dictionary]{dictionary} to retrieve the object.}

\item{...}{(any)\cr
Additional arguments.}

\item{.keys}{(\code{character()})\cr
Keys passed to the respective \link[mlr3misc:Dictionary]{dictionary} to retrieve multiple objects.}
}
\value{
\link[R6:R6Class]{R6::R6Class} object of the respective type, or a list of \link[R6:R6Class]{R6::R6Class} objects for the plural versions.
}
\description{
Functions to retrieve objects, set parameters and assign to fields in one go.
Relies on \code{\link[mlr3misc:dictionary_sugar_get]{mlr3misc::dictionary_sugar_get()}} to extract objects from the respective \link[mlr3misc:Dictionary]{mlr3misc::Dictionary}:
\itemize{
\item \code{fs()} for a \link{FSelector} from \link{mlr_fselectors}.
\item \code{fss()} for a list of \link[=FSelector]{FSelectors} from \link{mlr_fselectors}.
\item \code{trm()} for a \link[bbotk:Terminator]{bbotk::Terminator} from \link{mlr_terminators}.
\item \code{trms()} for a list of \link[bbotk:Terminator]{Terminators} from \link{mlr_terminators}.
}
}
\examples{
# random search fselector with batch size of 5
fs("random_search", batch_size = 5)

# run time terminator with 20 seconds
trm("run_time", secs = 20)
}
