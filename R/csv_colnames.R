

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

    # Identify numeric and non-numeric columns
    column_classes <- vapply(
      data,
      class,
      character(1))

    numeric_index <- column_classes %in% c('numeric', 'integer', 'logical')

    output$numeric_columns    <- colnames(data)[numeric_index]
    output$nonnumeric_columns <- colnames(data)[!numeric_index]

    # Check for binary variables
    binary_index <- vapply(
      data[numeric_index],
      FUN = function(x) all(x %in% c(0, 1, NA)) && any(x == 0) && any(x == 1),
      logical(1))

    output$binary_columns <- output$numeric_columns[binary_index]

    # Check for possible ID variables

    # Check for possible categorical variables
    categorical_index <- vapply(
      data[numeric_index],
      FUN = function(x) {

        # No more than 10 distinct values, AND
        length(unique(x)) <= 10 &&

        # All values are integer, AND
        all(x %% 1 == 0) &&

        # All values are codes with three or fewer digits
        all(x <= 999)
      },
      logical(1))

    output$categorical_columns <- output$numeric_columns[categorical_index]

    # Check for highly correlated numeric columns
    if (sum(numeric_index) > 2) {

      cor_matrix <- cor(data[numeric_index])

      high_cor_index <- abs(cor_matrix) > 0.9 & upper.tri(cor_matrix)

      high_cor_row_index <- row(cor_matrix)[high_cor_index]
      high_cor_col_index <- col(cor_matrix)[high_cor_index]

      high_cor_columns <- vector('list', sum(high_cor_index))

      for (i in seq_along(high_cor_row_index)) {

        high_cor_columns[[i]] <- c(rownames(cor_matrix)[high_cor_row_index[i]],
                               colnames(cor_matrix)[high_cor_col_index[i]])
      }
    }

    output$high_cor_columns <- high_cor_columns
  }

  return(output)
}
