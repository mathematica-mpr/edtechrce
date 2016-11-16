context('Randomize')

randomize_results <- randomize(
  data = randomize_data)

randomize_no_data <- randomize()

randomize_block <- randomize(
  data = randomize_data,
  block_id = 'schoolid')

randomize_block_missing <- randomize(
  data = randomize_data,
  block_id = 'fake')

randomize_seed <- randomize(
  data = randomize_data,
  seed = 1000)

randomize_p75 <- randomize(
  data = randomize_data,
  p = 0.75)

randomize_p_error <- randomize(
  data = randomize_data,
  p = 1.5)

randomize_baseline <- randomize(
  data = randomize_data,
  baseline_vars = 'yrs_exp')

test_that("no error message", {
  expect_null(randomize_results$error_message)
  expect_null(randomize_block$error_message)
  expect_null(randomize_seed$error_message)
  expect_null(randomize_p75$error_message)
  expect_null(randomize_baseline$error_message)
})

test_that("error_messages are correct", {
  expect_equal(randomize_no_data$error_message, 'The uploaded data could not be parsed as a CSV. Please check the format of the file.')
  expect_equal(randomize_block_missing$error_message, 'The block_id variable was not found in the data. Please check the data file and the specification of the variable.')
  expect_equal(randomize_p_error$error_message, 'The probability of being selected into the treatment group must be greater than 0 and less than 1.')
})

test_that("plot created", {
  expect_type(randomize_baseline$plot, 'character')
  expect_length(randomize_baseline$plot, 1)
})
