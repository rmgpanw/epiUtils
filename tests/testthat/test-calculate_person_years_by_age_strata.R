test_that("calculate_person_years_by_age_strata works with valid data", {
  
  # Create test data
  test_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  # Test basic functionality
  result <- calculate_person_years_by_age_strata(test_data)
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("age_group", "person_years", "events", "n"))
  expect_s3_class(result$age_group, "factor")
  expect_type(result$person_years, "double")
  expect_type(result$events, "integer")
  expect_type(result$n, "integer")
  
  # Check that person_years are positive
  expect_true(all(result$person_years >= 0))
  
})

test_that("calculate_person_years_by_age_strata validates input data", {
  
  # Test missing columns
  incomplete_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01"))
  )
  
  expect_error(
    calculate_person_years_by_age_strata(incomplete_data),
    "Missing required column"
  )
  
  # Test duplicate patient_id
  duplicate_data <- data.frame(
    patient_id = c(1, 1, 2),
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(duplicate_data),
    "Duplicate patient_id values found"
  )
  
  # Test invalid event_status
  invalid_event_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1, 0, 2) # Should be integer and only 0 or 1
  )
  
  expect_error(
    calculate_person_years_by_age_strata(invalid_event_data),
    "event_status must be of type integer"
  )
  
})

test_that("calculate_person_years_by_age_strata handles custom age cut points", {
  
  test_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  # Custom age groups
  custom_ages <- c(0, 30, 60, 100)
  result <- calculate_person_years_by_age_strata(test_data, custom_ages)
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) <= length(custom_ages) - 1) # At most n-1 age groups
  
})
