% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FSelectorBatch.R
\name{FSelectorBatch}
\alias{FSelectorBatch}
\title{Class for Batch Feature Selection Algorithms}
\description{
The \link{FSelectorBatch} implements the optimization algorithm.
}
\details{
\link{FSelectorBatch} is an abstract base class that implements the base functionality each fselector must provide.
A subclass is implemented in the following way:
\itemize{
\item Inherit from FSelectorBatch.
\item Specify the private abstract method \verb{$.optimize()} and use it to call into your optimizer.
\item You need to call \code{instance$eval_batch()} to evaluate design points.
\item The batch evaluation is requested at the \link{FSelectInstanceBatchSingleCrit}/\link{FSelectInstanceBatchMultiCrit} object \code{instance}, so each batch is possibly executed in parallel via \code{\link[mlr3:benchmark]{mlr3::benchmark()}}, and all evaluations are stored inside of \code{instance$archive}.
\item Before the batch evaluation, the \link[bbotk:Terminator]{bbotk::Terminator} is checked, and if it is positive, an exception of class \code{"terminated_error"} is generated.
In the latter case the current batch of evaluations is still stored in \code{instance}, but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
\item After such an exception was caught we select the best set from \code{instance$archive} and return it.
\item Note that therefore more points than specified by the \link[bbotk:Terminator]{bbotk::Terminator} may be evaluated, as the Terminator is only checked before a batch evaluation, and not in-between evaluation in a batch.
How many more depends on the setting of the batch size.
\item Overwrite the private super-method \code{.assign_result()} if you want to decide how to estimate the final set in the instance and its estimated performance.
The default behavior is: We pick the best resample experiment, regarding the given measure, then assign its set and aggregated performance to the instance.
}
}
\section{Private Methods}{

\itemize{
\item \code{.optimize(instance)} -> \code{NULL}\cr
Abstract base method. Implement to specify feature selection of your subclass.
See technical details sections.
\item \code{.assign_result(instance)} -> \code{NULL}\cr
Abstract base method. Implement to specify how the final feature subset is selected.
See technical details sections.
}
}

\section{Resources}{

There are several sections about feature selection in the \href{https://mlr3book.mlr-org.com}{mlr3book}.
\itemize{
\item Learn more about \href{https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html#the-fselector-class}{fselectors}.
}

The \href{https://mlr-org.com/gallery.html}{gallery} features a collection of case studies and demos about optimization.
\itemize{
\item Utilize the built-in feature importance of models with \href{https://mlr-org.com/gallery/optimization/2023-02-07-recursive-feature-elimination/}{Recursive Feature Elimination}.
\item Run a feature selection with \href{https://mlr-org.com/gallery/optimization/2023-02-01-shadow-variable-search/}{Shadow Variable Search}.
}
}

\section{Super class}{
\code{\link[mlr3fselect:FSelector]{mlr3fselect::FSelector}} -> \code{FSelectorBatch}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FSelectorBatch-new}{\code{FSelectorBatch$new()}}
\item \href{#method-FSelectorBatch-optimize}{\code{FSelectorBatch$optimize()}}
\item \href{#method-FSelectorBatch-clone}{\code{FSelectorBatch$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="format"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-format'><code>mlr3fselect::FSelector$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="help"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-help'><code>mlr3fselect::FSelector$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="print"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-print'><code>mlr3fselect::FSelector$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FSelectorBatch-new"></a>}}
\if{latex}{\out{\hypertarget{method-FSelectorBatch-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FSelectorBatch$new(
  id = "fselector_batch",
  param_set,
  properties,
  packages = character(),
  label = NA_character_,
  man = NA_character_
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier for the new instance.}

\item{\code{param_set}}{\link[paradox:ParamSet]{paradox::ParamSet}\cr
Set of control parameters.}

\item{\code{properties}}{(\code{character()})\cr
Set of properties of the fselector.
Must be a subset of \code{\link[mlr3:mlr_reflections]{mlr_reflections$fselect_properties}}.}

\item{\code{packages}}{(\code{character()})\cr
Set of required packages.
Note that these packages will be loaded via \code{\link[=requireNamespace]{requireNamespace()}}, and are not attached.}

\item{\code{label}}{(\code{character(1)})\cr
Label for this object.
Can be used in tables, plot and text output instead of the ID.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.
The referenced help package can be opened via method \verb{$help()}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FSelectorBatch-optimize"></a>}}
\if{latex}{\out{\hypertarget{method-FSelectorBatch-optimize}{}}}
\subsection{Method \code{optimize()}}{
Performs the feature selection on a \link{FSelectInstanceBatchSingleCrit} or \link{FSelectInstanceBatchMultiCrit} until termination.
The single evaluations will be written into the \link{ArchiveBatchFSelect} that resides in the \link{FSelectInstanceBatchSingleCrit} / \link{FSelectInstanceBatchMultiCrit}.
The result will be written into the instance object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FSelectorBatch$optimize(inst)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{inst}}{(\link{FSelectInstanceBatchSingleCrit} | \link{FSelectInstanceBatchMultiCrit}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{\link[data.table:data.table]{data.table::data.table()}}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FSelectorBatch-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FSelectorBatch-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FSelectorBatch$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
