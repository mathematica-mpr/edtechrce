context("Impact")

test_impact <- impact(
  data = matching_by_grade,
  outcome_var = 'post.test',
  treat_var = 'Treatment',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_by_grade <- impact(
  data = matching_by_grade,
  outcome_var = 'post.test',
  treat_var = 'Treatment',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_no_data <- impact(
  data = NULL,
  outcome_var = 'post.test',
  treat_var = 'Treatment',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_data_0_obs <- impact(
  data = data.frame(x = character(0)),
  outcome_var = 'post.test',
  treat_var = 'Treatment',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_no_treatment <- impact(
  data = matching_by_grade,
  outcome_var = 'post.test',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_no_outcome <- impact(
  data = matching_by_grade,
  treat_var = 'Treatment',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_treatment_continuous <- impact(
  data = matching_by_grade,
  outcome_var = 'post.test',
  treat_var = 'pre.test',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_missing_vars <- impact(
  data = matching_by_grade,
  outcome_var = 'post.test',
  treat_var = 'fake',
  control_vars = 'pre.test',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_impact_character_control_var <- impact(
  data = matching_by_grade,
  outcome_var = 'post.test',
  treat_var = 'Treatment',
  control_vars = 'anon.student.id',
  cluster_var = 'no cluster',
  grade_var = 'no grade',
  direction = 'increase',
  cutoff = 0.5,
  probability = 75)

test_that("no error message", {
  expect_null(test_impact$error_message)
  expect_null(test_impact_by_grade$error_message)
})

test_that("grade treated correctly", {
  expect_equal(length(test_impact$results_by_grade), 1)
  expect_equal(test_impact$results_by_grade[[1]]$title, "All grades combined")

  expect_equal(length(test_impact_by_grade$results_by_grade), 6)
  expect_equal(test_impact_by_grade$results_by_grade[[1]]$title, "Grade 1")
})

test_that("samples sizes are correct", {
  test_impact$results_by_grade
})

test_that("rope calculated correctly", {
  expect_equal(sum(unlist(test_impact$results_by_grade[[1]]$rope_probabilities)), 1)
})

test_that("error_messages are correct", {
  expect_equal(test_impact_no_data$error_message, "The uploaded data could not be parsed as a CSV. Please check the format of the file.")

  expect_equal(test_impact_data_0_obs$error_message, "The uploaded data file does not have any observations. Please check that the correct file was uploaded.")

  expect_equal(test_impact_no_treatment$error_message, 'No treatment variable is selected. Please select the variable that indicates which observations used the app/training.')

  expect_equal(test_impact_no_outcome$error_message, 'No outcome variable is selected. Please select the variable that indicates the outcome data.')

  expect_equal(test_impact_treatment_continuous$error_message, 'Some values of the treatment variable are not 0 or 1 (or missing). Please check that the correct variable is selected, and that the data file contains the correct value for that variable.')

  expect_equal(test_impact_missing_vars$error_message, 'The treatment variable specified (fake) does not exist in the data file. Check the data file and the selected treatment variable.')

  expect_equal(test_impact_character_control_var$error_message, 'One or more of the outcome and control variables contains text values. Matching variables should only contain numeric values. Please check that the correct matching variables are selected and that the data file contains the correct values. One common issue is including text missing codes in the data. These should be changed to blank or ".".')
})
