#' Title
#'
#' @param data CSV data file input
#' @param treat_var Character vector of length 1 indicating treatment variable
#' @param match_vars Character vector of length at least 1 indicating matching variables
#' @param grade_var Character vector of length 1 indicating grade variable. If NULL, analysis will not split data by grade. Defaults to NULL.
#'
#' @return A list containing an error message if applicable, and if no errors are found, matching results by grade (if grade_var is not specified, a single matching result is returned).
#' @export
#'
#' @examples
#' match_by_grade <- matching(
#' data = matching_by_grade,
#' treat_var = 'Treatment',
#' match_vars = 'pre.test',
#' grade_var = 'grade')
#'
#' @importFrom grDevices dev.cur dev.off png
#' @importFrom stats as.formula
#' @importFrom utils read.csv write.csv
#' @importFrom MatchIt matchit match.data
#' @importFrom checkbaseline CheckBaseline
matching <- function(
  data,
  treat_var = NULL,
  match_vars = NULL,
  grade_var = NULL) {

  require(base64enc)
  require(checkbaseline)
  require(MatchIt)

  # Try to read data
  if (!('data.frame' %in% class(data))) {
    data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE))
  }

  # checks
  error_message <- NULL

  if ('try-error' %in% class(data)) error_message <- 'The uploaded data could not be parsed as a CSV. Please check the format of the file.'

  else if (nrow(data) == 0) error_message <- 'The uploaded data file does not have any observations. Please check that the correct file was uploaded.'

  else if (is.null(treat_var)) error_message <- 'No treatment variable is selected. Please select the variable that indicates which observations used the app/training.'

  else if (is.null(match_vars)) error_message <- 'No matching variables are selected. Please select the variable(s) that should be used to match users with similar non-users.'

  else if (!all(match_vars %in% colnames(data))) error_message <- 'One or more matching variables do not exist in the data file. Check that you did not select the blank line at the top of the matching variable selector.'

  else if (!all(data[, treat_var] %in% c(0, 1, NA))) error_message <- 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.'

  else if (any(sapply(data[, match_vars], class) == 'character')) error_message <- 'One or more matching variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or ".".'

  output <- list(
    error_message = error_message
  )

  if (is.null(error_message)) {

    output$results_by_grade <- list()

    # Add id column to data
    data <- cbind(data, `..id..` = seq_len(nrow(data)))

    # Clean data
    model_vars <- intersect(
      c('..id..', treat_var, grade_var, match_vars),
      colnames(data))

    missing_indices <- lapply(data[, model_vars, drop=FALSE], is.na)
    missing_index   <- Reduce(`|`, missing_indices)

    data <- data[!missing_index, ]

    # Split data by grade if grade variable is specified, otherwise fake it.
    if (!is.null(grade_var) && grade_var %in% colnames(data)) {
      grade_index <- data[, grade_var]
    }
    else {
      grade_index <- rep('All', nrow(data))
    }

    data_by_grade <- by(
      data = data,
      INDICES = grade_index,
      FUN = function(x) x)

    # Matching
    # Loop instead of lapply to enable accurate progress tracking.

    match_formula <- as.formula(sprintf('%s ~ %s', treat_var, paste(match_vars, collapse = '+')))

    grades <- names(data_by_grade)

    for (grade_i in seq_along(grades)) {
      grade <- grades[grade_i]

      data_grade <- data_by_grade[[grade]]

      # Try first with optimal matching
      match_result <- matchit(
        match_formula,
        data = data_grade,
        method = 'nearest',
        ratio = 1)

      # Check for good balance - if it fails, start trying with calipers
      matched_data <- match.data(match_result)

      baseline_analysis <- CheckBaseline(
        raw.DF = data_grade,
        matched.DF = matched_data,
        treatment = treat_var,
        variables = match_vars)

      balance_table <- as.data.frame(baseline_analysis$balance.tbl)

      good_balance <- all(abs(balance_table$Standardized.bias[balance_table$Matching == 'Matched']) <= 0.25)

      if (!good_balance) {

        # Keep calipers as integers to avoid decimal comparisons - just divide by 100 when passing to matchit.
        caliper <- 100

        while (caliper >= 25 && !good_balance) {

          match_result <- matchit(
            match_formula,
            data = data_grade,
            method = 'nearest',
            caliper = caliper / 100,
            ratio = 1)

          matched_data <- match.data(match_result)

          baseline_analysis <- CheckBaseline(
            raw.DF = data_grade,
            matched.DF = matched_data,
            treatment = treat_var,
            variables = match_vars)

          balance_table <- as.data.frame(baseline_analysis$balance.tbl)

          good_balance <- all(abs(balance_table$Standardized.bias[balance_table$Matching == 'Matched']) <= 0.25)

          caliper <- caliper - 25
        }
      }

      n_full <- nrow(data_grade)
      n_matched <- nrow(matched_data)

      n_full_treat <- sum(data_grade[[treat_var]]==1, na.rm=TRUE)
      n_matched_treat <- sum(matched_data[[treat_var]]==1, na.rm=TRUE)

      std_bias <- abs(balance_table$Standardized.bias)

      unbalanced_index <- std_bias > 0.25 & balance_table$Matching == 'Matched'
      unbalanced_vars <- as.character(balance_table$Name[unbalanced_index])

      match_var_means <- lapply(
        matched_data[, match_vars, drop=FALSE],
        FUN = function(match_var, treat_var) {

          treat_mean <- mean(match_var[treat_var == 1L])
          comparison_mean <- mean(match_var[treat_var == 0L])

          list(
            overall = mean(match_var),
            intervention = treat_mean,
            comparison = comparison_mean,
            difference = treat_mean - comparison_mean)
        },
        treat_var = matched_data[, treat_var])

      temp_plot <- tempfile()
      png(temp_plot)
        print(baseline_analysis$baseline.plot)
      dev.off(which = dev.cur())

      plot_encoded <- base64enc::base64encode(temp_plot)

      title <- ifelse(length(grades) > 1,
        sprintf('Grade %s', grade),
        '')

      output$results_by_grade[[grade]] <- list(
        dropped_treatment_obs = n_full_treat - n_matched_treat,
        good_balance = good_balance,
        grade = grade,
        match_var_means = match_var_means,
        plot = plot_encoded,
        samples = list(
          n_full = n_full,
          n_matched = n_matched,
          n_full_treat = n_full_treat,
          n_matched_treat = n_matched_treat),
        title = title,
        unbalanced_vars = unbalanced_vars
      )

      # If good balance was achieved, append the matched data to the set to be downloaded.
      if (good_balance) {
        output$download_data <- rbind(output$download_data, matched_data)
      }
    }

    # Remove temporary id from download data.
    output$download_data$`..id..` <- NULL
  }

  output$download_file <- sprintf('matching-%s.csv', Sys.Date())

  write.csv(output$download_data, output$download_file, row.names = FALSE)

  output$download_data <- NULL

  return(output)
}
