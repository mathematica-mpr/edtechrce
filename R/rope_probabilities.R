#' rope_probabilities
#'
#' @param model The result of a call to easybayesian::stanlm
#' @param threshold A numeric cutoff value specified by the user.
#'
#' @return A list of three probabilities:
#' \code{less_than} is the probability that the parameter is less than the negative absolute value of the threshold.
#' \code{equal} is the probability that the parameter is within +/- absolute value of the threshold.
#' \code{greater_than} is the probability that the parameter is greater than the absolute value of the threshold.
#'
#' @export
#'
#' @examples
#'
rope_probabilities <- function(model, parameter, threshold) {
  posterior_samples <- model$posteriorSamples$posteriorSamplesBeta[[parameter]]

  list(
    less_than    = mean(postereior_samples < (-1 * abs(threshold)), na.rm=TRUE),
    equal        = mean(abs(posterior_samples) < abs(threshold), na.rm=TRUE),
    greater_than = mean(posterior_samples > abs(threshold), na.rm=TRUE)
  )
}
