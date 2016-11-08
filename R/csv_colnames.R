#' Title
#'
#' @param data CSV data file input
#'
#' @return A list containing an error message if applicable, and if no errors are found, the column names of the data set, as R would parse them. We do this to ensure the colnames presented as options in dashboards conform to how R would parse them. If not, it's possible a user would select a column name in the tool that then would not be found once the data and select column names are passed to other functions.
#' @export
#'
#' @examples
#' csv_colnames(matching_by_grade)
#'
#' @importFrom utils read.csv
csv_colnames <- function(data) {

  require(readr)

  data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE))

  error_message <- NULL

  if ('try-error' %in% class(data)) {
    error_message <- 'Error reading data. No column names returned.'
  }

  output <- list(
    error_message = error_message
  )

  if (is.null(error_message)) {
    output$colnames <- colnames(data)
  }

  return(output)
}
