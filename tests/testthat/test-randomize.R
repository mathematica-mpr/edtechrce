context('Randomize')

randomize_results <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50)

randomize_no_data <- randomize()

randomize_block <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  block_id = 'schoolid')

randomize_block_missing <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  block_id = 'fake')

randomize_seed <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  seed = 1000)

randomize_p75 <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 75)

randomize_p_error <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = -5)

randomize_p_error_2 <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 'not a number')

randomize_n_error <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'number',
  intervention_quantity = 0)

randomize_baseline <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  baseline_vars = 'yrs_exp')

randomize_block_baseline <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  block_id = 'schoolid',
  baseline_vars = 'yrs_exp')

randomize_baseline_error <- randomize(
  data = randomize_data,
  unit_id = 'tchid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  baseline_vars = c('schoolid', 'tchid'))

randomize_different_level <- randomize(
  data = randomize_data,
  unit_id = 'schoolid',
  intervention_type = 'percentage',
  intervention_quantity = 50,
  baseline_vars = 'yrs_exp')

test_that("no error message", {
  expect_null(randomize_results$error_message)
  expect_null(randomize_block$error_message)
  expect_null(randomize_seed$error_message)
  expect_null(randomize_p75$error_message)
  expect_null(randomize_baseline$error_message)
  expect_null(randomize_block_baseline$error_message)
})

test_that("error_messages are correct", {
  expect_equal(randomize_no_data$error_message, 'The uploaded data could not be parsed as a CSV. Please check the format of the file.')
  expect_equal(randomize_block_missing$error_message, 'The block_id variable (fake) was not found in the data. Please check the data file and the specification of the variable.')
  expect_equal(randomize_p_error$error_message, 'The percentage of records assigned to the intervention group must be greater than 0 and less than 100.')
  expect_equal(randomize_n_error$error_message, 'The number of records assigned to the intervention group must be greater than 0 and less than the number of total records in the data.')
  expect_equal(randomize_baseline_error$error_message, 'One or more baseline variables (schoolid, tchid) are in character format in your data. These columns should be formatted as numbers.')
})

test_that("plot created", {
  expect_type(randomize_baseline$results_by_block$`1`$plot, 'character')
  expect_length(randomize_baseline$results_by_block$`1`$plot, 1)

  plots_by_block <- sapply(randomize_block_baseline$results_by_block, `[[`, 'plot')
  expect_type(plots_by_block, 'character')
  expect_length(plots_by_block, length(randomize_block_baseline$results_by_block))
})

test_that("means calculated", {
  expect_is(randomize_baseline$results_by_block$`1`$baseline_var_means, 'list')
  expect_length(randomize_baseline$results_by_block$`1`$baseline_var_means, 1)
  expect_length(randomize_baseline$results_by_block$`1`$baseline_var_means$yrs_exp, 5)

  expect_is(randomize_block_baseline$results_by_block$A$baseline_var_means, 'list')
  expect_length(randomize_block_baseline$results_by_block$A$baseline_var_means, 1)
  expect_length(randomize_block_baseline$results_by_block$A$baseline_var_means$yrs_exp, 5)
})

test_that("effect sizes are present", {
  expect_is(randomize_baseline$results_by_block$`1`$baseline_var_means$yrs_exp, 'list')
  expect_true(!is.na(randomize_baseline$results_by_block$`1`$baseline_var_means$yrs_exp$effect_size))
})
