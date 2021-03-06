% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy.R
\name{get_energy_balance}
\alias{get_energy_balance}
\title{Get energy balance time-series}
\usage{
get_energy_balance(df)
}
\arguments{
\item{df}{data.frame or tibble, with columns \code{datetime}, \code{consumption}, \code{production}}
}
\value{
tibble or data.frame
}
\description{
Input data frame must have columns \code{consumption}, \code{production}.
Output data frame has extra columns \code{balance}, \code{local}, \code{imported}, \code{exported}.
Column \code{balance} is positive when there is more production than consumption.
}
