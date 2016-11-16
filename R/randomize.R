#' Title
#'
#' @param data CSV data file input
#' @param block_id character vector of length 1 indicating the name of a variable indicating blocks within which randomization should occur, instead of across the overall data set
#' @param seed optional integer vector of length 1 containing a seed value to ensure identical randomization results across more than one run
#' @param p optional numeric vector of length 1 containing a value greater than 0 and less than 1, indicating the probability of a single unit of analysis being assigned to the treatment group. Default value is 0.5.
#' @param baseline_vars optional character vector of numeric variables for which to produce a balance plot between the treatment and comparison groups after completing random assignment
#'
#' @return A list containing an error message if applicable, and if no errors are found, a file name containing the randomization results for download, and if baseline_vars are specified, the base64-encoded balance plot
#' @export
#'
#' @examples
#'
#' @importFrom grDevices dev.cur dev.off png
#' @importFrom utils read.csv write.csv
#' @importFrom checkbaseline CheckBaseline

randomize <- function(
  data = NULL,
  block_id = NULL,
  seed = NULL,
  p = 0.5,
  baseline_vars = NULL)
{

  require(checkbaseline)

  # Try to read data
  if (!('data.frame' %in% class(data))) {
    data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE))
  }

  # checks
  error_message <- NULL

  if ('try-error' %in% class(data)) error_message <- 'The uploaded data could not be parsed as a CSV. Please check the format of the file.'

  else if (nrow(data) == 0) error_message <- 'The uploaded data file does not have any observations. Please check that the correct file was uploaded.'

  else if (!is.null(block_id) && !(block_id %in% colnames(data))) error_message <- 'The block_id variable was not found in the data. Please check the data file and the specification of the variable.'

  else if (p <= 0 || p >= 1) error_message <- 'The probability of being selected into the treatment group must be greater than 0 and less than 1.'

  else if (!is.null(seed) && !is.numeric(seed)) error_message <- 'Seed must be a number.'

  else if (!is.null(baseline_vars) && !all(baseline_vars %in% colnames(data))) error_message <- 'One or more baseline variables specified were not found in the data set. Please check the data file and the specification of the baseline variables.'

  output <- list(
    error_message = error_message
  )

  if (is.null(error_message)) {

    try_status <- try({
      if (!is.null(seed)) set.seed(seed)

      if (is.null(block_id)) {
        block_id <- '..block_id..'
        data[, block_id] <- 1
      }

      data <- by(
        data = data,
        INDICES = data[, block_id],
        FUN = function(block_data) {

          block_data$Treatment <- sample(
            x = c(0, 1),
            size = nrow(block_data),
            replace = TRUE,
            prob = c(p, 1 - p))

          block_data
        }
      )

      data <- as.data.frame(do.call(rbind, data))

      if (!is.null(baseline_vars)) {

        baseline_analysis <- CheckBaseline(
          raw.DF = data,
          treatment = 'Treatment',
          variables = baseline_vars)

        temp_plot <- tempfile()
        png(temp_plot)
          print(baseline_analysis$baseline.plot)
        dev.off(which = dev.cur())

        output$plot <- base64enc::base64encode(temp_plot)
      }

    })

    if ('try-error' %in% class(try_status)) {
      output$error_message <- 'There was a problem producing random assignments for your data set, indicating there may be issues that will require a person to diagnose. Please contact a researcher for help, or contact the administrators of this website.'
    }
    else {
      output$download_file <- sprintf('randomize-%s.csv', Sys.Date())
      write.csv(data, output$download_file, row.names = FALSE)
    }
  }

  return(output)
}

