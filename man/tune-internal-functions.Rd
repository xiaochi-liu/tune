% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_helpers.R, R/grid_performance.R,
%   R/load_ns.R, R/logging.R
\name{forge_from_workflow}
\alias{forge_from_workflow}
\alias{finalize_workflow_preprocessor}
\alias{tune-internal-functions}
\alias{.estimate_metrics}
\alias{.load_namespace}
\alias{.catch_and_log}
\alias{.catch_and_log_fit}
\title{Internal functions used by other tidymodels packages}
\usage{
forge_from_workflow(new_data, workflow)

finalize_workflow_preprocessor(workflow, grid_preprocessor)

.estimate_metrics(dat, metric, param_names, outcome_name, event_level)

.load_namespace(x)

.catch_and_log(.expr, ..., bad_only = FALSE, notes)

.catch_and_log_fit(.expr, ..., notes)
}
\arguments{
\item{new_data}{A data frame or matrix of predictors to process.}

\item{workflow}{A workflow.}

\item{grid_preprocessor}{A tibble with parameter information.}

\item{dat}{A data set.}

\item{metric}{A metric set.}

\item{param_names}{A character vector of tuning parameter names.}

\item{outcome_name}{A character string for the column of \code{dat} that is the
outcome.}

\item{event_level}{A logical passed from the control function.}

\item{x}{A character vector of package names.}

\item{.expr}{Code to execute.}

\item{...}{Object to pass to the internal \code{tune_log()} function.}

\item{bad_only}{A logical for whether warnings and errors should be caught.}

\item{notes}{Character data to add to the logging.}
}
\description{
These are not to be meant to be invoked directly by users.
}
\keyword{internal}
