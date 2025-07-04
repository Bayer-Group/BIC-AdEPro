% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_number_ae_end_is_earlier_than_start.R
\name{calculate_number_ae_end_is_earlier_than_start}
\alias{calculate_number_ae_end_is_earlier_than_start}
\title{Calculate the Number of Cases when Adverse Event Start Day is after the Adverse Event End Day}
\usage{
calculate_number_ae_end_is_earlier_than_start(dat, sel_aestdy, sel_aeendy)
}
\arguments{
\item{dat}{data frame with adverse event data}

\item{sel_aestdy}{character with adverse event start day variable name}

\item{sel_aeendy}{character with adverse event end day variable name}
}
\value{
numeric value with the number of adverse event end day smaller start day
}
\description{
Calculate the Number of Cases when Adverse Event Start Day is after the Adverse Event End Day
}
