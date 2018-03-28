context("Create dummy variables")

test_no_data <- create_dummy_variables(
  data = NULL,
  categorical_columns = NULL)

test_numeric_data <- create_dummy_variables(
  data = 1:4,
  categorical_columns = NULL)

test_no_columns <- create_dummy_variables(
  data = matching_by_grade,
  categorical_columns = NULL)

test_numeric_columns <- create_dummy_variables(
  data = matching_by_grade,
  categorical_columns = 1:4)

test_missing_columns <- create_dummy_variables(
  data = matching_by_grade,
  categorical_columns = c('grade', 'missing'))

test_valid <- create_dummy_variables(
  data = matching_by_grade,
  categorical_columns = 'grade')

test_that('errors are caught', {
  expect_equal(test_no_data$error_message, '`data` must be a file path or data.frame.')
  expect_equal(test_numeric_data$error_message, '`data` must be a file path or data.frame.')
  expect_equal(test_no_columns$error_message, 'No `categorical_columns` specified.')
  expect_equal(test_numeric_columns$error_message, '`categorical_columns` must be a character array')
  expect_equal(test_missing_columns$error_message, 'The following columns were not found in the data: missing')
})

valid_output <- read.csv(test_valid$download_file,
                         header = TRUE,
                         stringsAsFactors = FALSE)

test_that('valid case works', {
  expect_null(test_valid$error_message)

  expect_true(all(paste0('grade_', 1:6) %in% colnames(valid_output)))
})
