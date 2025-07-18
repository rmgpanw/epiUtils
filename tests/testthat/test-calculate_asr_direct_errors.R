test_that("calculate_asr_direct input validation works correctly", {
  
  # Valid test data
  valid_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  # Test non-data.frame input
  expect_error(
    calculate_asr_direct("not a data frame"),
    "must be a data frame"
  )
  
  expect_error(
    calculate_asr_direct(list(a = 1, b = 2)),
    "must be a data frame"
  )
  
  # Test missing columns
  missing_age_group <- valid_data[, -1]  # Remove age_group
  expect_error(
    calculate_asr_direct(missing_age_group),
    "Missing required column.*age_group"
  )
  
  missing_events <- valid_data[, !names(valid_data) %in% "events"]
  expect_error(
    calculate_asr_direct(missing_events),
    "Missing required column.*events"
  )
  
  missing_person_years <- valid_data[, !names(valid_data) %in% "person_years"]
  expect_error(
    calculate_asr_direct(missing_person_years),
    "Missing required column.*person_years"
  )
  
  missing_standard_pop <- valid_data[, !names(valid_data) %in% "standard_pop"]
  expect_error(
    calculate_asr_direct(missing_standard_pop),
    "Missing required column.*standard_pop"
  )
  
  # Test multiple missing columns
  missing_multiple <- valid_data[, c("age_group", "events")]
  expect_error(
    calculate_asr_direct(missing_multiple),
    "Missing required columns.*person_years.*standard_pop"
  )
})

test_that("calculate_asr_direct handles missing values correctly", {
  
  # Test missing values in events
  data_missing_events <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, NA_integer_, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_missing_events),
    "Missing values found in column.*events"
  )
  
  # Test missing values in person_years
  data_missing_py <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, NA, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_missing_py),
    "Missing values found in column.*person_years"
  )
  
  # Test missing values in standard_pop
  data_missing_std <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, NA, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_missing_std),
    "Missing values found in column.*standard_pop"
  )
  
  # Test missing values in multiple columns
  data_missing_multiple <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, NA_integer_, 25L),
    person_years = c(1000, 1500, NA),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_missing_multiple),
    "Missing values found in columns.*events.*person_years"
  )
})

test_that("calculate_asr_direct validates data types correctly", {
  
  # Test non-integer events
  data_numeric_events <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5.0, 15.0, 25.0),  # numeric instead of integer
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_numeric_events),
    "Column 'events' must be of type integer"
  )
  
  # Test character events
  data_char_events <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c("5", "15", "25"),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_char_events),
    "Column 'events' must be of type integer"
  )
  
  # Test non-numeric person_years
  data_char_py <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c("1000", "1500", "2000"),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_char_py),
    "Column 'person_years' must be numeric"
  )
  
  # Test non-numeric standard_pop
  data_char_std <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c("1000", "1500", "2000")
  )
  
  expect_error(
    calculate_asr_direct(data_char_std),
    "Column 'standard_pop' must be numeric"
  )
})

test_that("calculate_asr_direct validates CI method parameter", {
  
  valid_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  # Test invalid CI method
  expect_error(
    calculate_asr_direct(valid_data, ci_method = "invalid"),
    "must be one of"
  )
  
  expect_error(
    calculate_asr_direct(valid_data, ci_method = "poisson"),
    "must be one of"
  )
  
  # Test that valid methods work
  expect_silent(
    calculate_asr_direct(valid_data, ci_method = "gamma", warn_small_cases = FALSE)
  )
  
  expect_silent(
    calculate_asr_direct(valid_data, ci_method = "byars", warn_small_cases = FALSE)
  )
})

test_that("calculate_asr_direct validates value ranges correctly", {
  
  # Test negative events
  data_negative_events <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(-1L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_negative_events),
    "Column 'events' must contain non-negative values"
  )
  
  # Test zero person_years
  data_zero_py <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(0, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_zero_py),
    "Column 'person_years' must contain positive values"
  )
  
  # Test negative person_years
  data_negative_py <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(-1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_negative_py),
    "Column 'person_years' must contain positive values"
  )
  
  # Test zero standard_pop
  data_zero_std <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(0, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_zero_std),
    "Column 'standard_pop' must contain positive values"
  )
  
  # Test negative standard_pop
  data_negative_std <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(-1000, 1500, 2000)
  )
  
  expect_error(
    calculate_asr_direct(data_negative_std),
    "Column 'standard_pop' must contain positive values"
  )
})

test_that("calculate_asr_direct validates parameter ranges", {
  
  valid_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  # Test invalid confidence levels
  expect_error(
    calculate_asr_direct(valid_data, conf_level = 0),
    "conf_level must be between 0 and 1"
  )
  
  expect_error(
    calculate_asr_direct(valid_data, conf_level = 1),
    "conf_level must be between 0 and 1"
  )
  
  expect_error(
    calculate_asr_direct(valid_data, conf_level = -0.5),
    "conf_level must be between 0 and 1"
  )
  
  expect_error(
    calculate_asr_direct(valid_data, conf_level = 1.5),
    "conf_level must be between 0 and 1"
  )
  
  # Test invalid multipliers
  expect_error(
    calculate_asr_direct(valid_data, multiplier = 0),
    "multiplier must be positive"
  )
  
  expect_error(
    calculate_asr_direct(valid_data, multiplier = -1000),
    "multiplier must be positive"
  )
  
  # Test valid edge cases
  expect_silent(
    calculate_asr_direct(valid_data, conf_level = 0.001, warn_small_cases = FALSE)
  )
  
  expect_silent(
    calculate_asr_direct(valid_data, conf_level = 0.999, warn_small_cases = FALSE)
  )
  
  expect_silent(
    calculate_asr_direct(valid_data, multiplier = 0.001, warn_small_cases = FALSE)
  )
})
