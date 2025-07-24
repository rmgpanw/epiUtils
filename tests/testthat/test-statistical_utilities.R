test_that("calculate_se_proportion works correctly", {
  # Test single values
  result <- calculate_se_proportion(p = 0.1, n = 100)
  expected <- 1 / (sqrt(2 * 0.1 * 0.9) * 100)
  expect_equal(result, expected)
  
  # Test multiple values with same length
  p_vals <- c(0.1, 0.2, 0.3)
  n_vals <- c(100, 200, 300)
  result <- calculate_se_proportion(p = p_vals, n = n_vals)
  expected <- 1 / (sqrt(2 * p_vals * (1 - p_vals)) * n_vals)
  expect_equal(result, expected)
  
  # Test broadcasting - single p, multiple n
  result <- calculate_se_proportion(p = 0.1, n = c(100, 200, 300))
  expected <- 1 / (sqrt(2 * 0.1 * 0.9) * c(100, 200, 300))
  expect_equal(result, expected)
  
  # Test broadcasting - multiple p, single n
  result <- calculate_se_proportion(p = c(0.1, 0.2, 0.3), n = 100)
  expected <- 1 / (sqrt(2 * c(0.1, 0.2, 0.3) * (1 - c(0.1, 0.2, 0.3))) * 100)
  expect_equal(result, expected)
})

test_that("calculate_se_proportion validates inputs correctly", {
  # Test p values outside (0, 1)
  expect_error(calculate_se_proportion(p = 0, n = 100),
               "Zero probability values are not allowed")
  expect_error(calculate_se_proportion(p = 1, n = 100),
               "values equal to 1.*not allowed")
  expect_error(calculate_se_proportion(p = -0.1, n = 100),
               "contains negative values")
  expect_error(calculate_se_proportion(p = 1.1, n = 100),
               "contains values > 1")
  
  # Test invalid sample sizes
  expect_error(calculate_se_proportion(p = 0.1, n = 0),
               "must be >= 1")
  expect_error(calculate_se_proportion(p = 0.1, n = -10),
               "must be >= 1")
  
  # Test length mismatch
  expect_error(calculate_se_proportion(p = c(0.1, 0.2), n = c(100, 200, 300)),
               "must have the same length")
})

test_that("calculate_z_score works correctly", {
  # Test single values
  result <- calculate_z_score(beta = 0.5, se = 0.2)
  expect_equal(result, 2.5)
  
  # Test multiple values
  beta_vals <- c(-0.3, 0.5, 1.2)
  se_vals <- c(0.15, 0.2, 0.4)
  result <- calculate_z_score(beta = beta_vals, se = se_vals)
  expected <- beta_vals / se_vals
  expect_equal(result, expected)
  
  # Test broadcasting
  result <- calculate_z_score(beta = c(0.5, 1.0), se = 0.2)
  expect_equal(result, c(2.5, 5.0))
  
  result <- calculate_z_score(beta = 0.5, se = c(0.1, 0.2))
  expect_equal(result, c(5.0, 2.5))
})

test_that("calculate_z_score validates inputs correctly", {
  # Test non-positive standard errors
  expect_error(calculate_z_score(beta = 0.5, se = 0),
               "Zero values are not allowed")
  expect_error(calculate_z_score(beta = 0.5, se = -0.1),
               "Negative values are not allowed")
  
  # Test length mismatch
  expect_error(calculate_z_score(beta = c(0.1, 0.2), se = c(0.1, 0.2, 0.3)),
               "must have the same length")
})

test_that("calculate_z_score_from_p works correctly", {
  # Test known values
  result <- calculate_z_score_from_p(p = 0.05)
  expected <- qnorm(1 - 0.05/2)  # Should be approximately 1.96
  expect_equal(result, expected)
  
  # Test multiple values
  p_vals <- c(0.001, 0.01, 0.05, 0.1)
  result <- calculate_z_score_from_p(p = p_vals)
  expected <- qnorm(1 - p_vals/2)
  expect_equal(result, expected)
  
  # Test extreme values
  expect_equal(calculate_z_score_from_p(p = 1), 0)
  expect_equal(calculate_z_score_from_p(p = 0), Inf)
})

test_that("calculate_z_score_from_p validates inputs correctly", {
  # Test invalid p values
  expect_error(calculate_z_score_from_p(p = -0.1),
               "contains negative values")
  expect_error(calculate_z_score_from_p(p = 1.1),
               "contains values > 1")
})

test_that("calculate_z_score_from_p warns about extreme values", {
  # Test warning for infinite Z-scores
  expect_warning(calculate_z_score_from_p(p = 0),
                 "non-finite values.*infinite Z")
})

test_that("calculate_se_from_z works correctly", {
  # Test single values
  result <- calculate_se_from_z(beta = 0.5, z = 2.5)
  expect_equal(result, 0.2)
  
  # Test multiple values
  beta_vals <- c(0.3, 0.5, 0.8)
  z_vals <- c(1.5, 2.5, 3.2)
  result <- calculate_se_from_z(beta = beta_vals, z = z_vals)
  expected <- abs(beta_vals / z_vals)  # abs to ensure positive
  expect_equal(result, expected)
  
  # Test broadcasting
  result <- calculate_se_from_z(beta = c(0.5, 1.0), z = 2.5)
  expect_equal(result, c(0.2, 0.4))
})

test_that("calculate_se_from_z handles sign inconsistencies", {
  # Test warning for negative standard errors
  expect_warning(calculate_se_from_z(beta = 0.5, z = -2.5),
                 "negative standard error.*inconsistent signs")
  
  # Test that result is positive even with sign mismatch
  result <- suppressWarnings(calculate_se_from_z(beta = 0.5, z = -2.5))
  expect_equal(result, 0.2)  # Should be positive
})

test_that("calculate_se_from_z validates inputs correctly", {
  # Test zero Z-scores
  expect_error(calculate_se_from_z(beta = 0.5, z = 0),
               "Zero values are not allowed")
  
  # Test length mismatch
  expect_error(calculate_se_from_z(beta = c(0.1, 0.2), z = c(1, 2, 3)),
               "must have the same length")
})

test_that("calculate_f_statistic works correctly", {
  # Test single values
  result <- calculate_f_statistic(beta = 0.5, se = 0.2)
  expected <- (0.5 / 0.2)^2
  expect_equal(result, expected)
  
  # Test multiple values
  beta_vals <- c(-0.3, 0.5, 1.2)
  se_vals <- c(0.15, 0.2, 0.4)
  result <- calculate_f_statistic(beta = beta_vals, se = se_vals)
  expected <- (beta_vals / se_vals)^2
  expect_equal(result, expected)
  
  # Test that F is always non-negative
  result <- calculate_f_statistic(beta = c(-0.5, 0.5), se = c(0.2, 0.2))
  expect_true(all(result >= 0))
})

test_that("calculate_f_statistic validates inputs correctly", {
  # Test non-positive standard errors
  expect_error(calculate_f_statistic(beta = 0.5, se = 0),
               "Zero values are not allowed")
  expect_error(calculate_f_statistic(beta = 0.5, se = -0.1),
               "Negative values are not allowed")
  
  # Test length mismatch
  expect_error(calculate_f_statistic(beta = c(0.1, 0.2), se = c(0.1, 0.2, 0.3)),
               "must have the same length")
})

test_that("relationship between Z and F statistics holds", {
  # Test F = Z^2 relationship
  beta <- c(-0.3, 0.5, 1.2)
  se <- c(0.15, 0.2, 0.4)
  
  z_scores <- calculate_z_score(beta = beta, se = se)
  f_stats <- calculate_f_statistic(beta = beta, se = se)
  
  expect_equal(f_stats, z_scores^2)
})

test_that("statistical utilities handle edge cases", {
  # Test very small effect sizes
  small_beta <- 1e-10
  se <- 0.1
  
  z <- calculate_z_score(beta = small_beta, se = se)
  expect_equal(z, small_beta / se)
  
  f <- calculate_f_statistic(beta = small_beta, se = se)
  expect_equal(f, (small_beta / se)^2)
  
  # Test very large effect sizes
  large_beta <- 100
  z <- calculate_z_score(beta = large_beta, se = se)
  expect_equal(z, large_beta / se)
})

test_that("consistency across different function combinations", {
  # Test round-trip calculations
  original_beta <- 0.5
  original_se <- 0.2
  
  # Calculate Z-score
  z <- calculate_z_score(beta = original_beta, se = original_se)
  
  # Calculate SE from Z and beta
  recovered_se <- calculate_se_from_z(beta = original_beta, z = z)
  
  expect_equal(recovered_se, original_se, tolerance = 1e-10)
  
  # Test F statistic consistency
  f <- calculate_f_statistic(beta = original_beta, se = original_se)
  expect_equal(f, z^2, tolerance = 1e-10)
})