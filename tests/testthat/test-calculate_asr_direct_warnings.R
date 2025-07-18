test_that("calculate_asr_direct handles small and zero case counts appropriately", {
  
  # Test data with mix of zero, small, and adequate case counts
  test_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60", "60-80"),
    events = c(0L, 3L, 15L, 25L),  # Zero, small, adequate, adequate
    person_years = c(1000, 1500, 2000, 1800),
    standard_pop = c(2500, 3000, 2000, 1500)
  )
  
  # Test with warnings enabled (default)
  expect_warning(
    result1 <- calculate_asr_direct(test_data),
    "Found 1 age group.*< 5 cases"
  )
  
  expect_message(
    result1 <- calculate_asr_direct(test_data),
    "Found 1 age group.*zero cases"
  )
  
  # Should still produce valid results
  expect_true(is.numeric(result1$asr))
  expect_true(is.numeric(result1$ci_lower))
  expect_true(is.numeric(result1$ci_upper))
  expect_true(result1$asr > 0)
  
  # Test with warnings disabled
  expect_silent(
    result2 <- calculate_asr_direct(test_data, warn_small_cases = FALSE)
  )
  
  # Results should be identical regardless of warning setting
  expect_equal(result1$asr, result2$asr)
  expect_equal(result1$ci_lower, result2$ci_lower)
  expect_equal(result1$ci_upper, result2$ci_upper)
  
  # Test that zero person-years still fails
  bad_data <- test_data
  bad_data$person_years[1] <- 0
  
  expect_error(
    calculate_asr_direct(bad_data),
    "person_years.*positive values"
  )
})

test_that("calculate_asr_direct warning messages are informative", {
  
  # Test data designed to trigger specific warning patterns
  single_small_case <- data.frame(
    age_group = c("0-20", "20-40"),
    events = c(2L, 15L),
    person_years = c(1000, 2000),
    standard_pop = c(1000, 2000)
  )
  
  multiple_small_cases <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(1L, 3L, 15L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1000, 1500, 2000)
  )
  
  # Test single small case warning
  expect_warning(
    calculate_asr_direct(single_small_case),
    "Found 1 age group.*< 5 cases.*0-20.*2 case"
  )
  
  # Test multiple small cases warning
  expect_warning(
    calculate_asr_direct(multiple_small_cases),
    "Found 2 age groups.*< 5 cases"
  )
  
  # Test informative guidance in warnings
  expect_warning(
    calculate_asr_direct(single_small_case),
    "Consider wider age groups"
  )
})
