test_that("calculate_person_years_by_age_strata input validation works correctly", {
  
  # Valid test data
  valid_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  # Test non-data.frame input
  expect_error(
    calculate_person_years_by_age_strata("not a data frame"),
    "must be a data frame"
  )
  
  expect_error(
    calculate_person_years_by_age_strata(list(a = 1, b = 2)),
    "must be a data frame"
  )
  
  expect_error(
    calculate_person_years_by_age_strata(matrix(1:10, nrow = 2)),
    "must be a data frame"
  )
})

test_that("calculate_person_years_by_age_strata handles missing columns correctly", {
  
  valid_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  # Test missing patient_id
  missing_id <- valid_data[, !names(valid_data) %in% "patient_id"]
  expect_error(
    calculate_person_years_by_age_strata(missing_id),
    "Missing required column.*patient_id"
  )
  
  # Test missing dob
  missing_dob <- valid_data[, !names(valid_data) %in% "dob"]
  expect_error(
    calculate_person_years_by_age_strata(missing_dob),
    "Missing required column.*dob"
  )
  
  # Test missing study_entry_date
  missing_entry <- valid_data[, !names(valid_data) %in% "study_entry_date"]
  expect_error(
    calculate_person_years_by_age_strata(missing_entry),
    "Missing required column.*study_entry_date"
  )
  
  # Test missing study_exit_date
  missing_exit <- valid_data[, !names(valid_data) %in% "study_exit_date"]
  expect_error(
    calculate_person_years_by_age_strata(missing_exit),
    "Missing required column.*study_exit_date"
  )
  
  # Test missing event_status
  missing_status <- valid_data[, !names(valid_data) %in% "event_status"]
  expect_error(
    calculate_person_years_by_age_strata(missing_status),
    "Missing required column.*event_status"
  )
  
  # Test multiple missing columns
  missing_multiple <- valid_data[, c("patient_id", "dob")]
  expect_error(
    calculate_person_years_by_age_strata(missing_multiple),
    "Missing required columns.*study_entry_date.*study_exit_date.*event_status"
  )
})

test_that("calculate_person_years_by_age_strata handles missing values correctly", {
  
  # Test missing values in patient_id
  data_missing_id <- data.frame(
    patient_id = c(1, NA, 3),
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_missing_id),
    "Missing values found in column.*patient_id"
  )
  
  # Test missing values in dob
  data_missing_dob <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", NA, "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_missing_dob),
    "Missing values found in column.*dob"
  )
  
  # Test missing values in study_entry_date
  data_missing_entry <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", NA, "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_missing_entry),
    "Missing values found in column.*study_entry_date"
  )
  
  # Test missing values in study_exit_date
  data_missing_exit <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", NA, "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_missing_exit),
    "Missing values found in column.*study_exit_date"
  )
  
  # Test missing values in event_status
  data_missing_status <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, NA_integer_, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_missing_status),
    "Missing values found in column.*event_status"
  )
  
  # Test missing values in multiple columns
  data_missing_multiple <- data.frame(
    patient_id = c(1, NA, 3),
    dob = as.Date(c("1960-01-15", "1955-06-20", NA)),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_missing_multiple),
    "Missing values found in columns.*patient_id.*dob"
  )
})

test_that("calculate_person_years_by_age_strata validates data types correctly", {
  
  # Test non-date dob
  data_char_dob <- data.frame(
    patient_id = 1:3,
    dob = c("1960-01-15", "1955-06-20", "1972-09-01"),  # character instead of Date
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_char_dob),
    "Column.*dob.*must be of class Date"
  )
  
  # Test non-date study_entry_date
  data_char_entry <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = c("2020-03-01", "2021-01-01", "2019-01-01"),  # character instead of Date
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_char_entry),
    "Column.*study_entry_date.*must be of class Date"
  )
  
  # Test non-date study_exit_date
  data_char_exit <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = c("2024-06-15", "2025-12-31", "2022-04-10"),  # character instead of Date
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_char_exit),
    "Column.*study_exit_date.*must be of class Date"
  )
  
  # Test non-integer event_status
  data_numeric_status <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1.0, 0.0, 1.0)  # numeric instead of integer
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_numeric_status),
    "Column.*event_status.*must be of type integer"
  )
})

test_that("calculate_person_years_by_age_strata validates event_status values", {
  
  # Test invalid event_status values (not 0 or 1)
  data_invalid_status <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 2L, 1L)  # 2 is invalid
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_invalid_status),
    "event_status must contain only 0.*or 1"
  )
  
  # Test negative event_status
  data_negative_status <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, -1L, 1L)  # -1 is invalid
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_negative_status),
    "event_status must contain only 0.*or 1"
  )
})

test_that("calculate_person_years_by_age_strata validates date relationships", {
  
  # Test study_exit_date before study_entry_date
  data_invalid_dates <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2019-06-15", "2025-12-31", "2022-04-10")),  # First date is before entry
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_invalid_dates),
    "Invalid follow-up time found"
  )
  
  # Test multiple patients with invalid date order
  data_multiple_invalid <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2019-06-15", "2020-12-31", "2018-04-10")),  # All invalid
    event_status = c(1L, 0L, 1L)
  )
  
  expect_error(
    calculate_person_years_by_age_strata(data_multiple_invalid),
    "Invalid follow-up time found"
  )
})

test_that("calculate_person_years_by_age_strata validates age cut points", {
  
  valid_data <- data.frame(
    patient_id = 1:3,
    dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
    event_status = c(1L, 0L, 1L)
  )
  
  # Test non-numeric age cut points
  expect_error(
    calculate_person_years_by_age_strata(valid_data, age_cut_points = c("0", "10", "20")),
    "age_cut_points must be a numeric vector"
  )
  
  # Test non-sorted age cut points
  expect_error(
    calculate_person_years_by_age_strata(valid_data, age_cut_points = c(10, 0, 20)),
    "age_cut_points must be in ascending order"
  )
  
  # Test empty age cut points
  expect_error(
    calculate_person_years_by_age_strata(valid_data, age_cut_points = numeric(0)),
    "age_cut_points must be a numeric vector with at least 2 values"
  )
  
  # Test single age cut point
  expect_error(
    calculate_person_years_by_age_strata(valid_data, age_cut_points = 0),
    "age_cut_points must be a numeric vector with at least 2 values"
  )
})

test_that("calculate_person_years_by_age_strata edge cases work correctly", {
  
  # Test with patients born after study start (should work)
  edge_data <- data.frame(
    patient_id = 1:2,
    dob = as.Date(c("2020-01-15", "2019-06-20")),  # Born during or close to study period
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31")),
    event_status = c(1L, 0L)
  )
  
  # Should not error - young patients are valid
  expect_silent(
    calculate_person_years_by_age_strata(edge_data)
  )
  
  # Test with very old patients (should work without warnings since default goes to 150)
  old_data <- data.frame(
    patient_id = 1:2,
    dob = as.Date(c("1920-01-15", "1925-06-20")),  # Very old patients (~95-100 years)
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31")),
    event_status = c(1L, 0L)
  )
  
  # Should not error or warn since ages 95-100 are within default range [0-150)
  expect_silent(
    calculate_person_years_by_age_strata(old_data)
  )
  
  # Test with extremely old patients that would exceed default range
  extremely_old_data <- data.frame(
    patient_id = 1:2,
    dob = as.Date(c("1870-01-15", "1875-06-20")),  # Extremely old patients (~145-150 years)
    study_entry_date = as.Date(c("2020-03-01", "2021-01-01")),
    study_exit_date = as.Date(c("2024-06-15", "2025-12-31")),
    event_status = c(1L, 0L)
  )
  
  # Should produce warnings about very old age groups exceeding default range
  expect_warning(
    calculate_person_years_by_age_strata(extremely_old_data),
    regex = ".*"  # Allow any warning
  )
})
