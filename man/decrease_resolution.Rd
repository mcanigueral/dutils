% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries.R
\name{decrease_resolution}
\alias{decrease_resolution}
\title{Decrease time resolution of timeseries data frame}
\usage{
decrease_resolution(df, resolution_mins, value = c("average", "first"))
}
\arguments{
\item{df}{data.frame or tibble, first column of name \code{datetime} being of class datetime and rest of columns being numeric}

\item{resolution_mins}{integer, interval of minutes between two consecutive datetime values}

\item{value}{character being 'average' or 'first'}
}
\value{
tibble
}
\description{
Decrease time resolution of timeseries data frame
}
