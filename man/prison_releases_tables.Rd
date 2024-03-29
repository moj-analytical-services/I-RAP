% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prison_releases_tables.R
\name{prison_releases_tables}
\alias{prison_releases_tables}
\title{Build a set of prison releases data tables in iRAP format}
\usage{
prison_releases_tables(dates, SHA = "main")
}
\arguments{
\item{dates}{date(s) of the file(s) you wish to use as source data files. Should be a character vector of dates in the format "yyyyQq"}
}
\value{
a list of data tables containing standard published data
}
\description{
This function builds a set of iRAP formatted data tables of prison receptions
It is a specific form of the iRAP_build_table() function with specified parameters for the standard published prison release tables
}
