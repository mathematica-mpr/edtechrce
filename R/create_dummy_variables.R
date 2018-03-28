#' Title
#'
#' @param data CSV data file input
#' @param categorical_columns Character vector of length at least 1 indicating names of categorical variables for which new dummy variables should be created.
#'
#' @return A list containing an error message if applicable, and if no errors are found, the name of a file created which can then be downloaded.
#' @export
#'
#' @examples
#' create_dummy_variables(
#'   data = matching_by_grade,
#'   categorical_columns = c('grade'))
#' @importFrom jsonlite toJSON
create_dummy_variables <- function(
  data = NULL,
  categorical_columns = NULL)
{
  data_file <- data

  error_message <- NULL

  if (is.null(data) || !(class(data) %in% c('character', 'data.frame'))) {
    error_message <- '`data` must be a file path or data.frame.'
  } else {

    # Try to read data
    if (!('data.frame' %in% class(data))) {
      data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE, na.strings = c('NA', '', '.')))
    }

    if ('try-error' %in% class(data)) {
      error_message <- 'Error reading `data` No column names returned.'
    } else if (is.null(categorical_columns)) {
      error_message <- 'No `categorical_columns` specified.'
    } else if (!is.character(categorical_columns)) {
      error_message <- '`categorical_columns` must be a character array'
    } else if (!all(categorical_columns %in% colnames(data))) {
      error_message <- sprintf('The following columns were not found in the data: %s',
                               paste0(setdiff(categorical_columns, colnames(data)), collapse = ', '))
    }
  }

  output <- list(
    error_message = error_message
  )

  if (is.null(error_message)) {

    try_status <- try({
      for (categorical_column in categorical_columns) {

        for (value in sort(unique(data[[categorical_column]]))) {

          dummy_name <- make.names(
            sprintf('%s_%s',
                    categorical_column,
                    value))

          data[[dummy_name]] <- as.numeric(
            !is.na(data[[categorical_column]]) &
            data[[categorical_column]] == value)

        }
      }

      if (class(data_file) == 'character') {
        output$download_file <- sub('\\.csv$', '_with_dummies.csv', data_file)
      } else {
        output$download_file <- sprintf('data_with_dummies-%s.csv', Sys.Date())
      }
      write.csv(data, output$download_file, row.names = FALSE)
    })

    if ('try-error' %in% class(try_status)) {
      output$error_message <- 'There was a problem reading the data file. Please contact a researcher for help, or contact the administrators of this website.'
    }
  }

  # Be sure output can be converted to JSON by jsonlite
  json_test <- try(jsonlite::toJSON(output))

  if (is(json_test, 'try-error')) output <- list(error_message = 'There was a problem converting output to JSON format.')

  return(output)
}
