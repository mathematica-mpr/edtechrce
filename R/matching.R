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
#' @importFrom stats as.formula aggregate var
#' @importFrom utils read.csv write.csv
#' @importFrom MatchIt matchit match.data
#' @importFrom checkbaseline CheckBaseline
#' @importFrom jsonlite toJSON
#' @importFrom methods is
matching <- function(
  data = NULL,
  treat_var = NULL,
  match_vars = NULL,
  grade_var = NULL) {

  require(base64enc)
  require(checkbaseline)
  require(MatchIt)

  data_file <- data

  # Try to read data
  if (!('data.frame' %in% class(data))) {
    data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE, na.strings = c('NA', '', '.')))
  }

  # checks
  error_message <- NULL

  if ('try-error' %in% class(data)) error_message <- 'The uploaded data could not be parsed as a CSV. Please check the format of the file.'

  else if (nrow(data) == 0) error_message <- 'The uploaded data file does not have any observations. Please check that the correct file was uploaded.'

  else if (is.null(treat_var)) error_message <- 'No treatment variable is selected. Please select the variable that indicates which observations used the app/training.'

  else if (is.null(match_vars)) error_message <- 'No matching variables are selected. Please select the variable(s) that should be used to match users with similar non-users.'

  else if (!all(match_vars %in% colnames(data))) error_message <- 'One or more matching variables do not exist in the data file. Check that you did not select the blank line at the top of the matching variable selector.'

  else if (!all(data[, treat_var] %in% c(0, 1, NA))) error_message <- 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.'

  else if (all(data[, treat_var] == 0L)) error_message <- 'No treatment observations found in data. Check data and specified treatment variable.'

  else if (all(data[, treat_var] == 1L)) error_message <- 'No comparison observations found in data. Check data and specified treatment variable.'

  else if (any(sapply(data[, match_vars], class) == 'character')) error_message <- 'One or more matching variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or "NA".'

  else if (!is.null(grade_var) && any(table(data[, grade_var]) < (length(treat_vars) + 2))) {

    error_message <- 'There are not enough observations in the data set to conduct matching in at least one grade.'

  } else if (is.null(grade_var) && (nrow(data) < length(treat_vars) + 2)) {

    error_message <- 'There are not enough observations in the data set to conduct matching.'

  }

  output <- list(
    error_message = error_message,
    args = list(
      treat_var = treat_var,
      match_vars = match_vars,
      grade_var = grade_var))

  if (is.null(error_message)) {

    output$results_by_grade <- list()

    try_status <- try({

      # Add id column to data
      data <- data_prematch <- cbind(data, `..id..` = seq_len(nrow(data)))

      match_vars <- unique(match_vars)

      # Clean data
      model_vars <- intersect(
        c('..id..', treat_var, grade_var, match_vars),
        colnames(data))

      missing_indices <- lapply(data[, model_vars, drop=FALSE], is.na)
      missing_index   <- Reduce(`|`, missing_indices)

      # Reduce to rows where model_vars aren't missing, and only model_vars, since matchit doesn't allow
      # any missing values in any columns. We'll merge non-model_vars back on at the end.
      data <- data[!missing_index, model_vars, drop=FALSE]

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
      match_formula <- as.formula(sprintf('`%s` ~ %s',
                                          treat_var,
                                          paste(
                                            sprintf('`%s`', match_vars),
                                            collapse = '+')))

      grades <- names(data_by_grade)

      for (grade_i in seq_along(grades)) {
        grade <- grades[grade_i]

        data_grade <- data_by_grade[[grade]]

        n_t <- sum(data_grade[[treat_var]] == 1)
        n_c <- sum(data_grade[[treat_var]] == 0)

        if (n_t == 0 || n_c == 0) {
          next
        } else {

          # From Ignacio, if we have more treatment than comparison obs,
          # reverse treatment indicator before matching and re-set afterwards.
          if (n_t > n_c) data_grade[[treat_var]] <- 1 - data_grade[[treat_var]]

          # Try first with optimal matching
          match_result <- matchit(
            match_formula,
            data = data_grade,
            method = 'nearest',
            ratio = 1)

          # Check for good balance - if it fails, start trying with calipers
          matched_data <- match.data(match_result)

          # If treatment groups were swapped, switch back before checking balance
          if (n_t > n_c) {
            data_grade[[treat_var]]   <- 1 - data_grade[[treat_var]]
            matched_data[[treat_var]] <- 1 - matched_data[[treat_var]]
          }

          baseline_analysis <- CheckBaseline(
            raw.DF = data_grade,
            matched.DF = matched_data,
            treatment = treat_var,
            variables = match_vars)

          balance_table <- as.data.frame(baseline_analysis$balance.tbl)

          bias <- balance_table$Standardized.bias[balance_table$Matching == 'Matched']

          na_bias <- is.na(bias) | is.infinite(bias)

          if (any(na_bias)) {
            dropped_vars <- match_vars[na_bias]

            baseline_analysis <- CheckBaseline(
              raw.DF = data_grade,
              matched.DF = matched_data,
              treatment = treat_var,
              variables = match_vars[!na_bias])

            balance_table <- as.data.frame(baseline_analysis$balance.tbl)

            bias <- balance_table$Standardized.bias[balance_table$Matching == 'Matched']

            na_bias <- is.na(bias) | is.infinite(bias)
          }
          else dropped_vars <- character(0)

          good_balance <- all(abs(bias) <= 0.25 | na_bias) && !all(na_bias)

          if (!good_balance) {

            # Keep calipers as integers to avoid decimal comparisons - just divide by 100 when passing to matchit.
            caliper <- 100

            while (caliper >= 25 && !good_balance) {

              # Repeat treatment group swap if necessary, since the original swap
              # was undone for the initial balance check
              if (n_t > n_c) data_grade[[treat_var]] <- 1 - data_grade[[treat_var]]

              match_result <- matchit(
                match_formula,
                data = data_grade,
                method = 'nearest',
                caliper = caliper / 100,
                ratio = 1)

              matched_data <- match.data(match_result)

              # If treatment groups were swapped, switch back before checking balance
              if (n_t > n_c) {
                data_grade[[treat_var]]   <- 1 - data_grade[[treat_var]]
                matched_data[[treat_var]] <- 1 - matched_data[[treat_var]]
              }

              baseline_analysis <- CheckBaseline(
                raw.DF = data_grade,
                matched.DF = matched_data,
                treatment = treat_var,
                variables = match_vars)

              balance_table <- as.data.frame(baseline_analysis$balance.tbl)

              bias <- balance_table$Standardized.bias[balance_table$Matching == 'Matched']

              na_bias <- is.na(bias)

              if (any(na_bias)) dropped_vars <- match_vars[na_bias]
              else dropped_vars <- character(0)

              good_balance <- all(abs(bias) <= 0.25 | is.na(bias)) && !all(na_bias)

              caliper <- caliper - 25
            }
          }

          n_full <- nrow(data_grade)
          n_matched <- nrow(matched_data)

          n_full_treat <- n_t
          n_matched_treat <- sum(matched_data[[treat_var]]==1, na.rm=TRUE)

          std_bias <- abs(balance_table$Standardized.bias)

          unbalanced_index <- std_bias > 0.25 & !is.na(std_bias) & !is.infinite(std_bias) & balance_table$Matching == 'Matched'
          unbalanced_vars <- as.character(balance_table$Name[unbalanced_index])

          baseline_vars <- setdiff(match_vars, dropped_vars)

          baseline_var_means <- lapply(
            matched_data[, baseline_vars, drop=FALSE],
            FUN = function(match_var, treat_var) {

              treat_index <- treat_var == 1L
              match_t <- match_var[treat_index]
              match_c <- match_var[!treat_index]

              mean_t <- mean(match_t)
              mean_c <- mean(match_c)

              n_t <- sum(treat_index)
              n_c <- sum(!treat_index)

              numerator <- (((n_t - 1) * var(match_t)) + ((n_c - 1) * var(match_c)))
              denominator <- n_t + n_c - 2

              s_pooled <- sqrt(numerator / denominator)

              difference <- mean_t - mean_c
              effect_size <- difference / s_pooled

              list(
                overall = mean(match_var),
                intervention = mean_t,
                comparison = mean_c,
                difference = difference,
                effect_size = effect_size)
            },
            treat_var = matched_data[, treat_var])

          temp_plot <- tempfile()
          png(temp_plot, width = 600, height = 480)
            print(baseline_analysis$baseline.plot)
          dev.off(which = dev.cur())

          plot_encoded <- base64enc::base64encode(temp_plot)

          title <- ifelse(length(grades) > 1,
            sprintf('Grade %s', grade),
            '')

          output$results_by_grade[[grade]] <- list(
            dropped_treatment_obs = n_full_treat - n_matched_treat,
            dropped_vars = dropped_vars,
            good_balance = good_balance,
            grade = grade,
            baseline_var_means = baseline_var_means,
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
      }
    })

    if ('try-error' %in% class(try_status)) {
      output$error_message <- 'There was a problem producing a matched data set, indicating there may be issues that will require a person to diagnose. Please contact a researcher for help, or contact the administrators of this website.'
    }
    else {
      output$download_data <- merge(
        output$download_data,
        data_prematch[, c('..id..', setdiff(colnames(data_prematch), colnames(output$download_data)))],
        by = '..id..')

      # Remove temporary id from download data.
      output$download_data$`..id..` <- NULL

      output$download_file <- sprintf('matching-%s.csv', Sys.Date())
      write.csv(output$download_data, output$download_file, row.names = FALSE)
      output$download_data <- NULL
    }

  }

  # Remove data file
  if (is.character(data_file) && file.exists(data_file)) file.remove(data_file)

  # Be sure output can be converted to JSON by jsonlite
  json_test <- try(toJSON(output))

  if (is(json_test, 'try-error')) output <- list(error_message = 'There was a problem converting output to JSON format.')

  return(output)
}
