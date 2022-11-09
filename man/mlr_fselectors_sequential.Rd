% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FSelectorSequential.R
\name{mlr_fselectors_sequential}
\alias{mlr_fselectors_sequential}
\alias{FSelectorSequential}
\title{Feature Selection via Sequential Search}
\description{
Sequential search iteratively adds features to the set.

Sequential forward selection (\code{strategy = fsf}) extends the feature set in each iteration with the feature that increases the models performance the most.
Sequential backward selection (\code{strategy = fsb}) follows the same idea but starts with all features and removes features from the set.

The feature selection terminates itself when \code{min_features} or \code{max_features} is reached.
It is not necessary to set a termination criterion.
}
\section{Dictionary}{

This \link{FSelector} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link{mlr_fselectors} or with the associated sugar function \code{\link[=fs]{fs()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_fselectors$get("sequential")
fs("sequential")
}\if{html}{\out{</div>}}
}

\section{Parameters}{

\describe{
\item{\code{min_features}}{\code{integer(1)}\cr
Minimum number of features. By default, 1.}
\item{\code{max_features}}{\code{integer(1)}\cr
Maximum number of features. By default, number of features in \link[mlr3:Task]{mlr3::Task}.}
\item{\code{strategy}}{\code{character(1)}\cr
Search method \code{sfs} (forward search) or \code{sbs} (backward search).}
}
}

\examples{
# retrieve task
task = tsk("pima")

# load learner
learner = lrn("classif.rpart")

\donttest{
# feature selection on the pima indians diabetes data set
instance = fselect(
  method = "sequential",
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)

# best performing feature subset
instance$result

# all evaluated feature subsets
as.data.table(instance$archive)

# subset the task and fit the final model
task$select(instance$result_feature_set)
learner$train(task)
}
}
\section{Super class}{
\code{\link[mlr3fselect:FSelector]{mlr3fselect::FSelector}} -> \code{FSelectorSequential}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FSelectorSequential-new}{\code{FSelectorSequential$new()}}
\item \href{#method-FSelectorSequential-optimization_path}{\code{FSelectorSequential$optimization_path()}}
\item \href{#method-FSelectorSequential-clone}{\code{FSelectorSequential$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="format"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-format'><code>mlr3fselect::FSelector$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="help"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-help'><code>mlr3fselect::FSelector$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="optimize"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-optimize'><code>mlr3fselect::FSelector$optimize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3fselect" data-topic="FSelector" data-id="print"><a href='../../mlr3fselect/html/FSelector.html#method-FSelector-print'><code>mlr3fselect::FSelector$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FSelectorSequential-new"></a>}}
\if{latex}{\out{\hypertarget{method-FSelectorSequential-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.`
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FSelectorSequential$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FSelectorSequential-optimization_path"></a>}}
\if{latex}{\out{\hypertarget{method-FSelectorSequential-optimization_path}{}}}
\subsection{Method \code{optimization_path()}}{
Returns the optimization path.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FSelectorSequential$optimization_path(inst, include_uhash = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{inst}}{(\link{FSelectInstanceSingleCrit})\cr
Instance optimized with \link{FSelectorSequential}.}

\item{\code{include_uhash}}{(\code{logical(1)})\cr
Include \code{uhash} column?}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{\link[data.table:data.table]{data.table::data.table()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FSelectorSequential-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FSelectorSequential-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FSelectorSequential$clone(deep = FALSE)}\if{html}{\out{</div>}}
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