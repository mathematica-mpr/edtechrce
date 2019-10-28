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
#' @importFrom utils read.csv write.csv
#' @importFrom checkbaseline CheckBaseline
#' @importFrom jsonlite toJSON
#' @importFrom methods is
randomize <- function(
  data = NULL,
  unit_id = NULL,
  seed = NULL,
  intervention_type = 'percentage', # 'percentage' or 'number'
  intervention_quantity = 50,
  block_id = NULL,
  baseline_vars = NULL)
{

  require(checkbaseline)

  data_file <- data

  # Try to read data
  if (!('data.frame' %in% class(data))) {
    data <- try(read.csv(data, header=TRUE, stringsAsFactors=FALSE))
  }

  # checks
  error_message <- NULL

  if ('try-error' %in% class(data)) error_message <- 'The uploaded data could not be parsed as a CSV. Please check the format of the file.'

  else if (nrow(data) == 0) error_message <- 'The uploaded data file does not have any observations. Please check that the correct file was uploaded.'

  else if (is.null(unit_id)) error_message <- 'The unit_id variable is required, but was not specified.'

  else if (!(unit_id %in% colnames(data))) error_message <- sprintf('The unit_id variable (%s) was not found in the data. Please check the data file and the specification of the variable.', unit_id)

  #else if (any(duplicated(data[, unit_id]))) error_message <- 'The data file should have one row per ID of the unit to be assigned. The uploaded file has duplicated values.'

  else if (!is.null(block_id) && block_id != '' && !(block_id %in% colnames(data))) error_message <- sprintf('The block_id variable (%s) was not found in the data. Please check the data file and the specification of the variable.', block_id)

  else if (is.null(intervention_type) || !(intervention_type %in% c('percentage', 'number'))) error_message <- 'Random assignment type was not specified or invalid. Valid values are "percentage" and "number".'

  else if (is.na(as.numeric(intervention_quantity))) error_message <- sprintf('The percentage or number of observations to be assigned to the treatment group (%s) could not be interpreted as a number.', intervention_quantity)

  else if (intervention_type == 'percentage' && (as.numeric(intervention_quantity) <= 0 || as.numeric(intervention_quantity) >= 100)) error_message <- 'The percentage of records assigned to the intervention group must be greater than 0 and less than 100.'

  else if (intervention_type == 'number' && (as.numeric(intervention_quantity) <= 0 || intervention_quantity >= nrow(data))) error_message <- 'The number of records assigned to the intervention group must be greater than 0 and less than the number of total records in the data.'

  else if (!is.null(seed) && !is.numeric(seed)) error_message <- 'Randomization seed must be a number.'

  else if (!is.null(baseline_vars) && !all(baseline_vars %in% colnames(data))) error_message <- sprintf('One or more baseline variables (%s) specified were not found in the data set. Please check the data file and the specification of the baseline variables.', paste(setdiff(baseline_vars, colnames(data)), collapse=', '))

  else if (any(sapply(data[, baseline_vars, drop=FALSE], class) == 'character'))  error_message <- sprintf('One or more baseline variables (%s) are in character format in your data. These columns should be formatted as numbers.', paste(baseline_vars[which(sapply(data[, baseline_vars, drop=FALSE], class) == 'character')], collapse=', '))

  else if ('Treatment' %in% colnames(data)) error_message <- 'The uploaded data file already has a column named "Treatment" - please remove this column and re-upload so the tool can add a Treatment group indicator to the file.'

  output <- list(
    error_message = error_message,
    args = list(
      unit_id = unit_id,
      seed = seed,
      intervention_type = intervention_type,
      intervention_quantity = intervention_quantity,
      block_id = block_id,
      baseline_vars = baseline_vars))

  if (is.null(error_message)) {

    try_status <- try({

      randomize_success <- FALSE
      randomize_attempts <- 0

      intervention_quantity <- as.numeric(intervention_quantity)

      # Try randomizing for up to 1 minute
      randomize_timeout <- FALSE
      start_time <- Sys.time()
      time_limit <- start_time + 60

      if (!is.null(seed)) set.seed(seed)

      while (!randomize_success && !randomize_timeout) {

        if (is.null(seed)) {
          iteration_seed <- as.integer(Sys.time()) + sample.int(1000:99999, size=1)
        } else {
          iteration_seed <- seed
        }

        set.seed(iteration_seed)

        # Create a dummy block_id if none was specified
        if (is.null(block_id)) {
          block_id <- '...block_id...'
          data[, block_id] <- 1
          dummy_block_id <- TRUE
        }
        else dummy_block_id <- FALSE

        baseline_vars <- unique(baseline_vars)

        results_by_block <- by(
          data = data,
          INDICES = data[, block_id],
          FUN = randomize_block,
          unit_id = unit_id,
          intervention_type = intervention_type,
          intervention_quantity = intervention_quantity,
          baseline_vars = baseline_vars)

        # Consider randomization a success if all blocks have good_balance == TRUE
        balance_by_block <- sapply(results_by_block, `[[`, 'good_balance')
        randomize_success <- all(is.na(balance_by_block) | balance_by_block)

        randomize_attempts <- randomize_attempts + 1
        randomize_timeout <- Sys.time() > time_limit
      }

      output$randomize_success  <- randomize_success
      output$randomize_attempts <- randomize_attempts
      output$randomize_timeout  <- randomize_timeout
      output$randomize_time     <- as.integer(Sys.time() - start_time)
      output$randomize_seed     <- iteration_seed

      # Re-assemble full data from randomized blocks
      randomized_data <- lapply(results_by_block, `[[`, 'data')
      randomized_data <- as.data.frame(do.call(rbind, randomized_data))

      # If a dummy block_id was used, remove it
      if (dummy_block_id) randomized_data[, block_id] <- NULL

      # Remove block-level data from results_by_block
      output$results_by_block <- lapply(
        results_by_block,
        FUN = function(result) {
          result$data <- NULL
          result
        })
    })

    if ('try-error' %in% class(try_status)) {
      output$error_message <- 'There was a problem producing random assignments for your data set, indicating there may be issues that will require a person to diagnose. Please contact a researcher for help, or contact the administrators of this website.'
    }
    else if (output$randomize_success) {
      output$download_file <- sprintf('randomize-%s-seed-%s.csv', Sys.Date(), iteration_seed)
      write.csv(randomized_data, output$download_file, row.names = FALSE)
      output$download_file <- NULL
    }
  }

  # Remove data file
  if (is.character(data_file) && file.exists(data_file)) file.remove(data_file)

  # Be sure output can be converted to JSON by jsonlite
  json_test <- try(toJSON(output))

  if (is(json_test, 'try-error')) output <- list(error_message = 'There was a problem converting output to JSON format.')

  return(output)
}


#' Title
#'
#' @param block_data
#' @param intervention_type
#' @param intervention_quantity
#' @param baseline_vars
#' @param unit_id
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom grDevices dev.cur dev.off png
#' @importFrom checkbaseline CheckBaseline
#' @importFrom stats aggregate var
randomize_block <- function(
  block_data,
  unit_id,
  intervention_type = 'percentage',
  intervention_quantity = 50,
  baseline_vars = NULL)
{
  units <- unique(block_data[, unit_id, drop=FALSE])

  if (intervention_type == 'percentage') {
    size <- ceiling((intervention_quantity / 100) * nrow(units))
  }
  else size <- intervention_quantity

  intervention_index <- sample.int(
                          n = nrow(units),
                          size = size)

  units$Treatment <- 0L
  units$Treatment[intervention_index] <- 1L

  block_data <- merge(
    block_data,
    units,
    all.x = TRUE)

  results <- list(
    data = block_data,
    good_balance = NA,
    samples = list(
      n_full = nrow(units),
      n_treat = size))

  if (!is.null(baseline_vars)) {

    baseline_analysis <- CheckBaseline(
      raw.DF = block_data,
      treatment = 'Treatment',
      variables = baseline_vars)

    balance_table <- as.data.frame(baseline_analysis$balance.tbl)
    results$good_balance <- all(abs(balance_table$Standardized.bias) <= 0.25)

    temp_plot <- tempfile()
    png(temp_plot, width = 600, height = 480)
      print(baseline_analysis$baseline.plot)
    dev.off(which = dev.cur())

    results$plot <- base64enc::base64encode(temp_plot)

    results$baseline_var_means <- lapply(
      block_data[, baseline_vars, drop=FALSE],
      FUN = function(
        baseline_var,
        treat_var)
      {
        # unit_level_means <- aggregate(
        #   list(var = baseline_var),
        #   by = list(unit_id = unit_id, treat_var = treat_var),
        #   FUN = mean,
        #   na.rm = TRUE)

        treat_index <- treat_var == 1L
        match_t <- baseline_var[treat_index]
        match_c <- baseline_var[!treat_index]

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
          overall = mean(baseline_var),
          intervention = mean_t,
          comparison = mean_c,
          difference = difference,
          effect_size = effect_size)

      },
      treat_var = block_data$Treatment)
  }

  results
}
