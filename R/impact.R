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
#' @importFrom stats as.formula coefficients lm var confint anova
#' @importFrom lme4 lmer fixef
#' @importFrom texreg htmlreg
#' @importFrom utils read.csv write.csv
#' @importFrom jsonlite toJSON
#' @importFrom methods is
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
  require(lme4)
  require(car)

  data_file <- data

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

  else if (is.null(control_vars) || length(control_vars) == 0L) error_message <- 'No control variables are selected. Please select at least one control variable. If you used matching to select your sample, use all variables you used for matching characteristics.'

  else if (!(treat_var %in% colnames(data))) error_message <- sprintf('The treatment variable specified (%s) does not exist in the data file. Check the data file and the selected treatment variable.', treat_var)

  else if (!all(control_vars %in% colnames(data))) error_message <- 'One or more control variables do not exist in the data file. Check that you did not select the blank line at the top of the matching variable selector.'

  else if (!all(data[[treat_var]] %in% c(0, 1, NA))) error_message <- 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.'

  else if (any(sapply(data[, c(outcome_var, control_vars)], class) == 'character')) error_message <- 'One or more of the outcome and control variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or ".".'

  else if (!is.null(direction) && !(direction %in% c('increase', 'decrease'))) error_message <- 'Intended direction of the effect must be either "increase" or "decrease".'

  else if (!is.null(cutoff) && is.na(as.numeric(cutoff)) && cutoff != '') error_message <- 'Cutoff value must be numeric.'

  output <- list(
    error_message = error_message,
    args = list(
      outcome_var = outcome_var,
      treat_var = treat_var,
      control_vars = control_vars,
      cluster_var = cluster_var,
      grade_var = grade_var,
      direction = direction,
      cutoff    = cutoff,
      probability = probability))

  if (is.null(error_message)) {

    try_status <- try({

      output$control_vars <- control_vars
      output$results_by_grade <- list()

      # If any of the arguments that are created in other tools were not specified, assume defaults. That's because users can use this tool outside the context of an evaluation. The impact estimates are still valid, just without context about the user's goals for success.
      if (is.null(direction))              direction <- 'increase'
      if (is.null(cutoff) || cutoff == '') cutoff <- 0

      cutoff <- as.numeric(cutoff)
      probability <- as.numeric(probability)

      # When direction == 'decrease', the cutoff needs to be treated as negative for the posterior plots and interpretation. If the cutoff is already 0 or negative, no need to change it.
      if (direction == 'decrease' && cutoff > 0) cutoff <- cutoff * -1

      # Based on email from Steve Bates 2016-12-01 at 8:19 AM, we want to set probability to 75 for the credible interval rather than allowing it to be parameterized.
      if (is.null(probability) || is.na(as.numeric(probability)) || probability != 75) probability <- 75

      # Remove missing values
      check_vars <- intersect(c(outcome_var, treat_var, control_vars, grade_var, cluster_var), colnames(data))
      data <- na.omit(data[, check_vars])

      output$outcome_range <- list(
        min = min(data[, outcome_var], na.rm=TRUE),
        max = max(data[, outcome_var], na.rm=TRUE))

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

      for (grade_i in seq_along(grades)) {
        grade <- grades[grade_i]

        grade_data <- data_by_grade[[grade]]

        # Check for covariates with no variation before constructing impact formula.
        if (length(control_vars) > 0) {

          covariate_variances <- sapply(
            grade_data[, control_vars, drop=FALSE],
            FUN = var,
            na.rm = TRUE)

          keep_index <- which(covariate_variances > 0 & !is.na(covariate_variances))

          control_vars_model <- control_vars[keep_index]

          # Also check for collinearity, any other issue that would make a covariate drop out of a linear model
          if (length(control_vars_model) > 0) {
            lm_formula <- sprintf('%s ~ %s', outcome_var, paste(control_vars_model, collapse=' + '))

            lm_result <- lm(lm_formula, data = grade_data)

            na_coefficients <- is.na(lm_result$coefficients)

            if (any(na_coefficients)) {
              keep_vars <- names(lm_result$coefficients)[!na_coefficients]
              control_vars_model <- intersect(control_vars_model, keep_vars)
            }
          }

        }
        else control_vars_model <- control_vars

        # Create impact formula
        impact_formula_string <- sprintf('`%s` ~ %s',
                                         outcome_var,
                                         paste(
                                           sprintf('`%s`', c(treat_var, control_vars_model)), collapse='+'))

        impact_formula <- as.formula(impact_formula_string)

        impact_clustered <- !is.null(cluster_var) && cluster_var %in% colnames(data)

        if (impact_clustered) {
          impact_formula_clustered <- as.formula(
            sprintf('%s + (1 | %s)', impact_formula_string, cluster_var))
        }

        if (impact_clustered) {

          bayesian_lm1 <- stanlm(impact_formula, cluster = cluster_var, data = grade_data, credible = probability / 100)

        } else {

          bayesian_lm1 <- stanlm(impact_formula, data = grade_data, credible = probability / 100)

        }

        # Check for divergent transitions, if present, re-run with adjusted parameters
        divergent_transitions <- sum(do.call(rbind, get_sampler_params(bayesian_lm1$fit, inc_warmup = FALSE))[, 'divergent__'])

        if (divergent_transitions > 0) {

          message('Divergent transitions detected, running with adjust iter and adapt_delta')

          if (impact_clustered) {

            bayesian_lm1 <- stanlm(impact_formula, cluster = cluster_var, data = grade_data, credible = probability / 100, iter = 4000, adapt_delta = 0.9999, stepsize = 0.001, max_treedepth = 20)

          } else {

            bayesian_lm1 <- stanlm(impact_formula, data = grade_data, credible = probability / 100, iter = 4000, adapt_delta = 0.9999, stepsize = 0.001, max_treedepth = 20)

          }
        }

        # Check again for divergent transitions warning, if present, stop with an error message
        divergent_transitions <- sum(do.call(rbind, get_sampler_params(bayesian_lm1$fit, inc_warmup = FALSE))[, 'divergent__'])

        if (divergent_transitions > 0) output$error_message <- 'There was a problem producing impact results (divergent transitions detected after setting iter = 4000 and adapt_delta = 0.9999), indicating there may be issues that will require a person to diagnose. Please contact a researcher for help, or contact the administrators of this website.'
        else {

          impact <- mean(bayesian_lm1$posteriorSamples$posteriorSamplesBeta[[treat_var]], na.rm=TRUE)

          rope_output <- get_rope_output(
            model = bayesian_lm1,
            parameter = treat_var,
            rope_threshold = cutoff,
            probability_threshold = probability)

          rope_bar_plot <- tempfile()
          png(rope_bar_plot)
            # grid.draw is required here because the plot object is converted to
            # a gtable to allow printing text outside the plot margin
            print(grid.draw(rope_output$plots$bar))
          dev.off(which = dev.cur())

          rope_output$plots$bar <- base64enc::base64encode(rope_bar_plot)

          rope_dist_plot <- tempfile()
          png(rope_dist_plot)
            print(grid.draw(rope_output$plots$dist))
          dev.off(which = dev.cur())

          rope_output$plots$dist <- base64enc::base64encode(rope_dist_plot)

          if (multiple_grades) title <- sprintf('Grade %s', grade)
          else title <- 'All grades combined'

          # Calculate frequentist model as well. Results will go in brief appendix.
          freq_try <- try({
            if (impact_clustered) {

              freq_lmer1 <- lmer(impact_formula_clustered, data = grade_data)

              # extract the point esitmate
              freq_impact <- fixef(freq_lmer1)[treat_var]

              freq_se     <- summary(freq_lmer1)$coef[treat_var, 'Std. Error']
              freq_pvalue <- car::Anova(freq_lmer1)[treat_var, 'Pr(>Chisq)']

              # compute the confidence interval
              freq_ci <- confint(freq_lmer1, treat_var)
              freq_lb <- freq_ci[1]
              freq_ub <- freq_ci[2]
            }
            else {
              freq_lm1    <- lm(impact_formula, data = grade_data)
              freq_coef   <- coefficients(summary(freq_lm1))
              freq_impact <- freq_coef[treat_var, 'Estimate']

              freq_se     <- freq_coef[treat_var, 'Std. Error']
              freq_pvalue <- freq_coef[treat_var, 'Pr(>|t|)']

              ci_width <- qt(p = 0.975, df = freq_lm1$df.residual) * freq_se
              freq_lb  <- freq_impact - ci_width
              freq_ub  <- freq_impact + ci_width
            }

            treat_index <- grade_data[[treat_var]] == 1
            outcome_t <- grade_data[[outcome_var]][treat_index]
            outcome_c <- grade_data[[outcome_var]][!treat_index]

            n_t <- sum(treat_index)
            n_c <- sum(!treat_index)

            numerator <- (((n_t - 1) * var(outcome_t)) + ((n_c - 1) * var(outcome_c)))
            denominator <- n_t + n_c - 2

            s_pooled <- sqrt(numerator / denominator)
            freq_effect_size <- freq_impact / s_pooled

            freq_results <- list(
              outcome     = outcome_var,
              impact      = freq_impact,
              effect_size = freq_effect_size,
              se          = freq_se,
              pvalue      = freq_pvalue,
              lb          = freq_lb,
              ub          = freq_ub)
          })

          if (is(freq_try, 'try-error')) {
            freq_results <- list(
              outcome     = outcome_var,
              impact      = NA,
              effect_size = NA,
              se          = NA,
              pvalue      = NA,
              lb          = NA,
              ub          = NA)

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

          # Control variable means - if no control vars are specified, will return NULL (in JSON, {})
          baseline_var_means <- lapply(control_vars,

            FUN = function(
              control_var,
              treat_var,
              grade_data,
              control_vars_model) {

              if (control_var %in% control_vars_model) {

                control_var <- grade_data[[control_var]]
                treat_var   <- grade_data[[treat_var]]

                treat_index <- treat_var == 1L
                match_t <- control_var[treat_index]
                match_c <- control_var[!treat_index]

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
                  overall = mean(control_var),
                  intervention = mean_t,
                  comparison = mean_c,
                  difference = difference,
                  effect_size = effect_size)

              } else {
                list(
                  overall = NA,
                  intervention = NA,
                  comparison = NA,
                  difference = NA,
                  effect_size = NA
                )
              }
            },
            treat_var = treat_var,
            grade_data = grade_data,
            control_vars_model = control_vars_model)

          # lapply where is.null(names(x)) returns an unnamed list. Add names for easier
          # detection of which variable is which in baseline_var_means.
          names(baseline_var_means) <- control_vars

          treat_index <- grade_data[[treat_var]] == 1L

          mean_t <- mean(grade_data[[outcome_var]][treat_index], na.rm=TRUE)
          mean_c <- mean(grade_data[[outcome_var]][!treat_index], na.rm=TRUE)

          samples <- list(
            n_full = nrow(grade_data),
            n_full_treat = sum(grade_data[[treat_var]]))

          if (impact_clustered) {
            samples_cluster <- list(
              n_full = length(unique(grade_data[[cluster_var]])),
              n_full_treat = length(unique(grade_data[[cluster_var]][treat_index]))
            )
          } else samples_cluster <- NULL

          output$results_by_grade[[grade]] <- list(
            baseline_var_means = baseline_var_means,
            grade = grade,
            freq = freq_results,
            impact = impact,
            interpretation = interpretation,
            interpretation_cutoff_0 = interpretation_cutoff_0,
            posterior_plot = base64enc::base64encode(posterior_plot),
            outcome_means = list(
              intervention = mean_t,
              comparison = mean_c),
            regression_table = regression_table,
            rope_output = rope_output,
            samples = samples,
            samples_cluster = samples_cluster,
            title = title,
            trace_plot = base64enc::base64encode(trace_plot))

        }
      }
    })

    output$outcome_var <- outcome_var

    if ('try-error' %in% class(try_status)) {
      output$error_message <- 'There was a problem producing impact results, indicating there may be issues that will require a person to diagnose. Please contact a researcher for help, or contact the administrators of this website.'
    }
  }

  # Remove data file
  if (is.character(data_file) && file.exists(data_file)) file.remove(data_file)

  # Be sure output can be converted to JSON by jsonlite
  json_test <- try(toJSON(output))

  if (is(json_test, 'try-error')) output <- list(error_message = 'There was a problem converting output to JSON format.')

  return(output)
}
