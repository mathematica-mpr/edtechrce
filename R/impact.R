#' Title
#'
#' @param data CSV data file input
#' @param outcome_var Character vector of length 1 indicating outcome variable
#' @param treat_var Character vector of length 1 indicating treatment variable
#' @param control_vars Character vector indicating control variables (optional)
#' @param cluster_var Character vector indicating a clustering variable (optional)
#' @param grade_var Character vector of length 1 indicating grade variable. If NULL, analysis will not split data by grade. Defaults to NULL.
#' @param direction The user-entered direction of intended impact.
#' @param cutoff The user-entered cutoff at which they would consider a program to have "moved the needle."
#' @param probability The user-entered minimum probability of the estimated impact being greater than the cutoff value to consider the program to have "moved the needle."
#'
#' @return A list containing an error message if applicable, and if no errors are found, impact results by grade (if grade_var is not specified, a single matching result is returned).
#' @export
#'
#' @examples
#'
#' @importFrom easybayesian interpret posteriorplot stanlm
#' @importFrom grDevices dev.cur dev.off png
#' @importFrom stats as.formula coefficients lm
#' @importFrom texreg htmlreg
#' @importFrom utils read.csv write.csv
impact <- function(
  data = NULL,
  outcome_var = NULL,
  treat_var = NULL,
  control_vars = NULL,
  cluster_var = NULL,
  grade_var = NULL,
  direction = NULL,
  cutoff    = NULL,
  probability = 75)
{

  require(rstan)
  require(easybayesian)
  require(base64enc)
  require(sandwich)
  require(lmtest)

  # Try to read data
  if (!('data.frame' %in% class(data))) {
    data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE))
  }

  # checks
  error_message <- NULL

  if ('try-error' %in% class(data)) error_message <- 'The uploaded data could not be parsed as a CSV. Please check the format of the file.'

  else if (nrow(data) == 0) error_message <- 'The uploaded data file does not have any observations. Please check that the correct file was uploaded.'

  else if (is.null(outcome_var)) error_message <- 'No outcome variable is selected. Please select the variable that indicates the outcome data.'

  else if (is.null(treat_var)) error_message <- 'No treatment variable is selected. Please select the variable that indicates which observations used the app/training.'

  else if (!all(control_vars %in% colnames(data))) error_message <- 'One or more control variables do not exist in the data file. Check that you did not select the blank line at the top of the matching variable selector.'

  else if (!all(data[[treat_var]] %in% c(0, 1, NA))) error_message <- 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.'

  else if (any(sapply(data[, c(outcome_var, control_vars)], class) == 'character')) error_message <- 'One or more of the outcome and control variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or ".".'

  else if (!is.null(direction) && !(direction %in% c('increase', 'decrease'))) error_message <- 'Intended direction of the effect must be either "increase" or "decrease".'

  else if (!is.null(cutoff) && is.na(as.numeric(cutoff))) error_message <- 'Cutoff value must be numeric.'

  output <- list(
    error_message = error_message
  )

  if (is.null(error_message)) {

    output$results_by_grade <- list()

    # If any of the arguments that are created in other tools were not specified, assume defaults. That's because users can use this tool outside the context of an evaluation. The impact estimates are still valid, just without context about the user's goals for success.
    if (is.null(direction))   direction <- 'increase'
    if (is.null(cutoff))      cutoff <- 0

    cutoff <- as.numeric(cutoff)
    probability <- as.numeric(probability)

    # Based on email from Steve Bates 2016-12-01 at 8:19 AM, we want to set probability to 75 for the credible interval rather than allowing it to be parameterized.
    if (is.null(probability) || is.na(as.numeric(probability)) || probability != 75) probability <- 75

    try_status <- try({

      # Split data by grade
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

      grades <- names(data_by_grade)
      n_grades <- length(grades)
      multiple_grades <- n_grades > 1

      # Create impact formula
      impact_formula <- as.formula(sprintf('%s ~ %s', outcome_var, paste(c(treat_var, control_vars), collapse='+')))


      for (grade_i in seq_along(grades)) {
        grade <- grades[grade_i]

        grade_data <- data_by_grade[[grade]]

        if (cluster_var %in% colnames(data)) {
          bayesian_lm1 <- try(stanlm(impact_formula, cluster = cluster_var, data = grade_data, credible = probability / 100))
        } else {
          bayesian_lm1 <- try(stanlm(impact_formula, data = grade_data, credible = probability / 100))
        }

        if (multiple_grades) title <- sprintf('Grade %s', grade)
        else title <- 'All grades combined'

        # Calculate frequentist model as well. Results will go in brief appendix.
        freq_lm1    <- try(lm(impact_formula, data = grade_data))

        if (!('try-error' %in% class(freq_lm1))) {
          freq_coef   <- coefficients(summary(freq_lm1))
          freq_impact <- freq_coef[treat_var, 'Estimate']

          if (cluster_var == 'no cluster') {
            freq_se     <- freq_coef[treat_var, 'Std. Error']
            freq_pvalue <- freq_coef[treat_var, 'Pr(>|t|)']
          }
          else {

            freq_cluster <- clustered.se(
              model_result = freq_lm1,
              data = grade_data,
              cluster = as.character(cluster_var),
              Tvar = as.character(treat_var),
              level = 0.95)

            freq_se     <- freq_cluster$standard.errors[treat_var]
            freq_pvalue <- freq_cluster$p.values[treat_var]
          }

          freq_lm1 <- list(
            outcome = outcome_var,
            impact= freq_impact,
            se    = freq_se,
            pvalue = freq_pvalue)
        }

        trace <- bayesian_lm1$traceplots

        posterior <- posteriorplot(
          model = bayesian_lm1,
          parameter = treat_var,
          cutoff = cutoff,
          credibleIntervalWidth = probability / 100,
          lessthan = (direction == 'decrease'))

        interpretation <- interpret(model = bayesian_lm1,
                  name = treat_var,
                  cutoff = cutoff,
                  credible = probability / 100,
                  lessthan = (direction == 'decrease'))

        if (cutoff == 0) interpretation_cutoff_0 <- interpretation
        else {
          interpretation_cutoff_0 <- interpret(model = bayesian_lm1,
                  name = treat_var,
                  cutoff = 0,
                  credible = probability / 100,
                  lessthan = (direction == 'decrease'))
        }

        # Save trace and posterior plots to tempfiles, will be written to database and inserted into brief appendix as base64 encoded text.
        trace_plot <- tempfile()
        png(trace_plot)
          print(trace)
        dev.off(which = dev.cur())

        posterior_plot <- tempfile()
        png(posterior_plot)
          print(posterior)
        dev.off(which = dev.cur())

        model_note <- sprintf("&#42 0 outside the %s%% credible interval.<br>The log posterior quantifies the combined posterior density of all model parameters.<br>R&#770 is the potential scale reduction factor on split chains (at convergence, R&#770 = 1).<br>N<sub>eff<//sub> is a crude measure of effective sample size.", probability)

        model_name <- sprintf('Point Estimate<br>[%s%% CI]', probability)

        regression_table <- htmlreg(bayesian_lm1$tbl,
                              star.symbol = '&#42',
                              custom.note = model_note,
                              custom.columns = list(
                                `R&#770` = bayesian_lm1$custom.columns$Rhat,
                                `N<sub>eff<//sub>` = bayesian_lm1$custom.columns$n_eff),
                              caption = '',
                              custom.model.names = model_name,
                              doctype = FALSE)

        output$results_by_grade[[grade]] <- list(
          grade = grade,
          freq = freq_lm1,
          interpretation = interpretation,
          interpretation_cutoff_0 = interpretation_cutoff_0,
          posterior_plot = base64enc::base64encode(posterior_plot),
          regression_table = regression_table,
          title = title,
          trace_plot = base64enc::base64encode(trace_plot))
      }
    })

    if ('try-error' %in% class(try_status)) {
      output$error_message <- 'There was a problem producing impact results, indicating there may be issues that will require a person to diagnose. Please contact a researcher for help, or contact the administrators of this website.'
    }
  }

  return(output)
}
