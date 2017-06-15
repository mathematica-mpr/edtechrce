context("Matching")

test_match <- matching(
  data = matching_by_grade,
  treat_var = 'Treatment',
  match_vars = 'pre.test')

test_match_by_grade <- matching(
  data = matching_by_grade,
  treat_var = 'Treatment',
  match_vars = 'pre.test',
  grade_var = 'grade')

test_match_no_data <- matching(
  data = NULL,
  treat_var = 'Treatment',
  match_vars = 'pre.test')

test_match_data_0_obs <- matching(
  data = data.frame(x = character(0)),
  treat_var = 'Treatment',
  match_vars = 'pre.test')

test_match_no_treatment <- matching(
  data = matching_by_grade,
  match_vars = 'pre.test')

test_match_no_match_vars <- matching(
  data = matching_by_grade,
  treat_var = 'Treatment')

test_match_treatment_continuous <- matching(
  data = matching_by_grade,
  treat_var = 'grade',
  match_vars = 'pre.test')

test_match_missing_vars <- matching(
  data = matching_by_grade,
  treat_var = 'nothing',
  match_vars = 'fake')

test_match_character_match_var <- matching(
  data = matching_by_grade,
  treat_var = 'Treatment',
  match_vars = 'anon.student.id')

matching_unusual_colnames <- matching_by_grade
matching_unusual_colnames$`var with spaces` <- matching_unusual_colnames$Treatment
matching_unusual_colnames$`1var with leading number` <- matching_unusual_colnames$pre.test

test_match_unusual_colnames <- matching(
  data = matching_unusual_colnames,
  treat_var = 'var with spaces',
  match_vars = '1var with leading number')

matching_no_users <- matching_by_grade
matching_no_users$Treatment <- 0

test_match_no_users <- matching(
  data = matching_no_users,
  treat_var = 'Treatment',
  match_vars = 'pre.test')

matching_all_users <- matching_by_grade
matching_all_users$Treatment <- 1

test_match_all_users <- matching(
  data = matching_all_users,
  treat_var = 'Treatment',
  match_vars = 'pre.test')

test_that("no error message", {
  expect_null(test_match$error_message)
  expect_null(test_match_by_grade$error_message)
  expect_null(test_match_unusual_colnames$error_message)
})

test_that("grade treated correctly", {
  expect_equal(length(test_match$results_by_grade), 1)
  expect_equal(test_match$results_by_grade[[1]]$title, "")

  expect_equal(length(test_match_by_grade$results_by_grade), 6)
  expect_equal(test_match_by_grade$results_by_grade[[1]]$title, "Grade 1")
})

test_that("samples sizes are correct", {
  test_match$results_by_grade
})

test_that("effect sizes are present", {
  baseline_var_means <- test_match$results_by_grade[[1]]$baseline_var_means
  effect_sizes <- sapply(baseline_var_means, `[[`, 'effect_size')

  expect_true(is.numeric(effect_sizes) & !is.na(effect_sizes))
})

test_that("all columns are returned in output data", {
  output_data <- read.csv(test_match$download_file, header=TRUE, stringsAsFactors = FALSE)
  expect_true(all(colnames(matching_by_grade) %in% colnames(output_data)))
})


test_that("error_messages are correct", {
  expect_equal(test_match_no_data$error_message, "The uploaded data could not be parsed as a CSV. Please check the format of the file.")

  expect_equal(test_match_data_0_obs$error_message, "The uploaded data file does not have any observations. Please check that the correct file was uploaded.")

  expect_equal(test_match_no_treatment$error_message, 'No treatment variable is selected. Please select the variable that indicates which observations used the app/training.')

  expect_equal(test_match_no_match_vars$error_message, 'No matching variables are selected. Please select the variable(s) that should be used to match users with similar non-users.')

  expect_equal(test_match_treatment_continuous$error_message, 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.')

  expect_equal(test_match_missing_vars$error_message, 'One or more matching variables do not exist in the data file. Check that you did not select the blank line at the top of the matching variable selector.')

  expect_equal(test_match_character_match_var$error_message, 'One or more matching variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or "NA".')

  expect_equal(test_match_no_users$error_message, 'No treatment observations found in data. Check data and specified treatment variable.')

  expect_equal(test_match_all_users$error_message, 'No comparison observations found in data. Check data and specified treatment variable.')
})



