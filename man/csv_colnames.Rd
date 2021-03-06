% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csv_colnames.R
\name{csv_colnames}
\alias{csv_colnames}
\title{Title}
\usage{
csv_colnames(data)
}
\arguments{
\item{data}{CSV data file input}
}
\value{
A list containing an error message if applicable, and if no errors are found, the column names of the data set, as R would parse them. We do this to ensure the colnames presented as options in dashboards conform to how R would parse them. If not, it's possible a user would select a column name in the tool that then would not be found once the data and select column names are passed to other functions.
}
\description{
Title
}
\examples{
csv_colnames(matching_by_grade)

}
