% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numericInputsModule.R
\name{numericInputsServer}
\alias{numericInputsServer}
\title{Server function of numericInput module}
\usage{
numericInputsServer(id, inputs_conf, sliders = FALSE, delay = 0)
}
\arguments{
\item{id}{character, module ID}

\item{inputs_conf}{tibble, numericInputs parameters table with columns
\code{input} (numericInput ID), \code{label}, \code{value}, \code{min}, \code{max}, \code{step} and \code{show}}

\item{sliders}{boolean, whether to use sliders or numeric inputs}

\item{delay}{integer, number of milliseconds to delay an input change}
}
\value{
named list with reactive input values
}
\description{
Server function of numericInput module
}
