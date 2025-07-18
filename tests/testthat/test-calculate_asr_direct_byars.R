test_that("calculate_asr_direct Byar's CI method works correctly", {
  
  # Test data with moderate case counts (should use normal approximation)
  moderate_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60", "60-80"),
    events = c(5L, 15L, 25L, 30L),  # Total = 75 cases (>10)
    person_years = c(1000, 1500, 2000, 1800),
    standard_pop = c(1500, 2000, 2500, 2000)
  )
  
  result_byars <- calculate_asr_direct(moderate_data, ci_method = "byars", warn_small_cases = FALSE)
  result_gamma <- calculate_asr_direct(moderate_data, ci_method = "gamma", warn_small_cases = FALSE)
  
  # Point estimates should be identical
  expect_equal(result_byars$asr, result_gamma$asr)
  expect_equal(result_byars$asr_scaled, result_gamma$asr_scaled)
  
  # Confidence intervals should be positive and reasonable
  expect_true(result_byars$ci_lower > 0)
  expect_true(result_byars$ci_upper > result_byars$ci_lower)
  expect_true(result_byars$ci_lower_scaled > 0)
  expect_true(result_byars$ci_upper_scaled > result_byars$ci_lower_scaled)
  
  # ASR should be within the confidence interval
  expect_true(result_byars$asr >= result_byars$ci_lower)
  expect_true(result_byars$asr <= result_byars$ci_upper)
})

test_that("calculate_asr_direct Byar's CI method handles small case counts", {
  
  # Test data with very small case counts (should use Poisson approach)
  small_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(0L, 2L, 1L),  # Total = 3 cases (<10)
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1500, 2000, 2500)
  )
  
  result_byars <- calculate_asr_direct(small_data, ci_method = "byars", warn_small_cases = FALSE)
  result_gamma <- calculate_asr_direct(small_data, ci_method = "gamma", warn_small_cases = FALSE)
  
  # Point estimates should be identical
  expect_equal(result_byars$asr, result_gamma$asr)
  
  # Confidence intervals should be positive and reasonable
  expect_true(result_byars$ci_lower >= 0)  # Can be 0 for very small counts
  expect_true(result_byars$ci_upper > result_byars$ci_lower)
  
  # Results should be valid numbers
  expect_false(is.na(result_byars$ci_lower))
  expect_false(is.na(result_byars$ci_upper))
  expect_false(is.infinite(result_byars$ci_lower))
  expect_false(is.infinite(result_byars$ci_upper))
})

test_that("calculate_asr_direct Byar's CI method handles zero total cases", {
  
  # Test data with zero total cases
  zero_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(0L, 0L, 0L),  # Total = 0 cases
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1500, 2000, 2500)
  )
  
  result_byars <- calculate_asr_direct(zero_data, ci_method = "byars", warn_small_cases = FALSE)
  
  # ASR should be zero
  expect_equal(result_byars$asr, 0)
  expect_equal(result_byars$asr_scaled, 0)
  
  # Lower CI should be zero
  expect_equal(result_byars$ci_lower, 0)
  expect_equal(result_byars$ci_lower_scaled, 0)
  
  # Upper CI should be positive or zero (can be zero for zero total cases)
  expect_true(result_byars$ci_upper >= 0)
  expect_true(result_byars$ci_upper_scaled >= 0)
})

test_that("calculate_asr_direct Byar's CI method handles edge case at boundary", {
  
  # Test data with exactly 10 cases (boundary between methods)
  boundary_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60", "60-80"),
    events = c(2L, 2L, 3L, 3L),  # Total = 10 cases (boundary)
    person_years = c(1000, 1500, 2000, 1800),
    standard_pop = c(1500, 2000, 2500, 2000)
  )
  
  result_byars <- calculate_asr_direct(boundary_data, ci_method = "byars", warn_small_cases = FALSE)
  
  # Should produce valid results
  expect_true(is.finite(result_byars$asr))
  expect_true(is.finite(result_byars$ci_lower))
  expect_true(is.finite(result_byars$ci_upper))
  
  # Confidence intervals should be reasonable
  expect_true(result_byars$ci_lower >= 0)
  expect_true(result_byars$ci_upper > result_byars$ci_lower)
})

test_that("calculate_asr_direct Byar's CI respects different confidence levels", {
  
  test_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60"),
    events = c(5L, 15L, 25L),
    person_years = c(1000, 1500, 2000),
    standard_pop = c(1500, 2000, 2500)
  )
  
  # Test different confidence levels with Byar's method
  result_90 <- calculate_asr_direct(test_data, ci_method = "byars", conf_level = 0.90, warn_small_cases = FALSE)
  result_95 <- calculate_asr_direct(test_data, ci_method = "byars", conf_level = 0.95, warn_small_cases = FALSE)
  result_99 <- calculate_asr_direct(test_data, ci_method = "byars", conf_level = 0.99, warn_small_cases = FALSE)
  
  # Point estimates should be identical
  expect_equal(result_90$asr, result_95$asr)
  expect_equal(result_95$asr, result_99$asr)
  
  # Higher confidence levels should have wider intervals
  width_90 <- result_90$ci_upper - result_90$ci_lower
  width_95 <- result_95$ci_upper - result_95$ci_lower
  width_99 <- result_99$ci_upper - result_99$ci_lower
  
  expect_true(width_90 < width_95)
  expect_true(width_95 < width_99)
  
  # All should contain the point estimate
  expect_true(result_90$asr >= result_90$ci_lower && result_90$asr <= result_90$ci_upper)
  expect_true(result_95$asr >= result_95$ci_lower && result_95$asr <= result_95$ci_upper)
  expect_true(result_99$asr >= result_99$ci_lower && result_99$asr <= result_99$ci_upper)
})

test_that("calculate_asr_direct Byar's CI method ensures non-negative lower bounds", {
  
  # Test with very small rates that might produce negative lower bounds
  very_small_data <- data.frame(
    age_group = c("0-20", "20-40"),
    events = c(1L, 1L),  # Very small counts
    person_years = c(100000, 100000),  # Large populations -> very small rates
    standard_pop = c(50000, 50000)
  )
  
  result_byars <- calculate_asr_direct(very_small_data, ci_method = "byars", warn_small_cases = FALSE)
  
  # Lower bound should never be negative
  expect_true(result_byars$ci_lower >= 0)
  expect_true(result_byars$ci_lower_scaled >= 0)
  
  # Should still be valid
  expect_true(result_byars$ci_upper > result_byars$ci_lower)
})

test_that("calculate_asr_direct CI methods produce similar results for large samples", {
  
  # Test with large case counts where methods should converge
  large_data <- data.frame(
    age_group = c("0-20", "20-40", "40-60", "60-80", "80+"),
    events = c(50L, 150L, 250L, 300L, 200L),  # Total = 950 cases
    person_years = c(10000, 15000, 20000, 18000, 12000),
    standard_pop = c(15000, 20000, 25000, 20000, 10000)
  )
  
  result_gamma <- calculate_asr_direct(large_data, ci_method = "gamma", warn_small_cases = FALSE)
  result_byars <- calculate_asr_direct(large_data, ci_method = "byars", warn_small_cases = FALSE)
  
  # Point estimates should be identical
  expect_equal(result_gamma$asr, result_byars$asr)
  
  # Confidence intervals should be similar (within 5% relative difference)
  relative_diff_lower <- abs(result_gamma$ci_lower - result_byars$ci_lower) / result_gamma$ci_lower
  relative_diff_upper <- abs(result_gamma$ci_upper - result_byars$ci_upper) / result_gamma$ci_upper
  
  expect_true(relative_diff_lower < 0.05)
  expect_true(relative_diff_upper < 0.05)
})
