% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rope_output - Copy.R, R/get_rope_output.R
\name{get_rope_output}
\alias{get_rope_output}
\alias{get_rope_output}
\title{get_rope_output}
\usage{
get_rope_output(model, parameter, rope_threshold, probability_threshold)

get_rope_output(model, parameter, rope_threshold, probability_threshold)
}
\arguments{
\item{model}{The result of a call to easybayesian::stanlm}

\item{parameter}{The name of the treatment variable in the analysis.}

\item{rope_threshold}{The cutoff threshold specified by the user.}

\item{probability_threshold}{The probability threshold specified by the user.}

\item{model}{The result of a call to easybayesian::stanlm}

\item{parameter}{The name of the treatment variable in the analysis.}

\item{rope_threshold}{The cutoff threshold specified by the user.}

\item{probability_threshold}{The probability threshold specified by the user.}
}
\value{
A list containing \code{probabilities} and \code{plots}:
\code{probabilities} contains three probabilities:
\code{less_than} is the probability that the parameter is less than the negative absolute value of the threshold.
\code{equal} is the probability that the parameter is within +/- absolute value of the threshold.
\code{greater_than} is the probability that the parameter is greater than the absolute value of the threshold.

\code{plots} contains two plots:
\code{bar}: A bar plot illustrating each of the values in \code{return_object$probabilities}
\code{dist}: A density plot illustrating the density of the parameter samples, colored by groups below, in, and above the region of practical equivalence (ROPE).

A list containing \code{probabilities} and \code{plots}:
\code{probabilities} contains three probabilities:
\code{less_than} is the probability that the parameter is less than the negative absolute value of the threshold.
\code{equal} is the probability that the parameter is within +/- absolute value of the threshold.
\code{greater_than} is the probability that the parameter is greater than the absolute value of the threshold.

\code{plots} contains two plots:
\code{bar}: A bar plot illustrating each of the values in \code{return_object$probabilities}
\code{dist}: A density plot illustrating the density of the parameter samples, colored by groups below, in, and above the region of practical equivalence (ROPE).
}
\description{
get_rope_output

get_rope_output
}
\examples{


}
