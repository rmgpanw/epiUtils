# Tests comparing with epitools::ageadjust.direct() ----

test_that("calculate_asr_direct matches epitools::ageadjust.direct results", {

  # Data from Fleiss, 1981, p. 249
  population_raw <- c(230061, 329449, 114920, 39487, 14208, 3052,
                      72202, 326701, 208667, 83228, 28466, 5375,
                      15050, 175702, 207081, 117300, 45026, 8660,
                      2293, 68800, 132424, 98301, 46075, 9834,
                      327, 30666, 123419, 149919, 104088, 34392,
                      319933, 931318, 786511, 488235, 237863, 61313)

  population_matrix <- matrix(population_raw, 6, 6,
                              dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                              c("1", "2", "3", "4", "5+", "Total")))

  count_raw <- c(107, 141, 60, 40, 39, 25,
                 25, 150, 110, 84, 82, 39,
                 3, 71, 114, 103, 108, 75,
                 1, 26, 64, 89, 137, 96,
                 0, 8, 63, 112, 262, 295,
                 136, 396, 411, 428, 628, 530)
  count_matrix <- matrix(count_raw, 6, 6,
                         dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                         c("1", "2", "3", "4", "5+", "Total")))

  # Use average population as standard
  standard_pop_epitools <- apply(population_matrix[, -6], 1, mean)

  # Test each 'birth order' column separately
  prep_data_for_epiutils <- function(cases_vec, pop_vec, std_pop_vec) {
    data.frame(
      age_group = rownames(population_matrix)[1:6], # Use original age group names
      events = as.integer(cases_vec),
      person_years = pop_vec,
      standard_pop = std_pop_vec
    )
  }

  # --- Birth Order 1 ---
  df1 <- prep_data_for_epiutils(count_matrix[, 1], population_matrix[, 1], standard_pop_epitools)

  our_result1 <- calculate_asr_direct(
    .df = df1,
    multiplier = 1, # Epitools returns raw rates, then multiplied later
    warn_small_cases = FALSE # Suppress warnings for test validation
  )
  epitools_result1 <- epitools::ageadjust.direct(
    count = count_matrix[, 1],
    pop = population_matrix[, 1],
    stdpop = standard_pop_epitools
  )

  # Check ASR (mean)
  expect_equal(our_result1$asr, as.numeric(epitools_result1["adj.rate"]), tolerance = 1e-6)
  # Check lower CI
  expect_equal(our_result1$ci_lower, as.numeric(epitools_result1["lci"]), tolerance = 1e-6)
  # Check upper CI
  expect_equal(our_result1$ci_upper, as.numeric(epitools_result1["uci"]), tolerance = 1e-6)

  # --- Birth Order 2 ---
  df2 <- prep_data_for_epiutils(count_matrix[, 2], population_matrix[, 2], standard_pop_epitools)
  our_result2 <- calculate_asr_direct(
    .df = df2,
    multiplier = 1,
    warn_small_cases = FALSE
  )
  epitools_result2 <- epitools::ageadjust.direct(
    count = count_matrix[, 2],
    pop = population_matrix[, 2],
    stdpop = standard_pop_epitools
  )
  expect_equal(our_result2$asr, as.numeric(epitools_result2["adj.rate"]), tolerance = 1e-6)
  expect_equal(our_result2$ci_lower, as.numeric(epitools_result2["lci"]), tolerance = 1e-6)
  expect_equal(our_result2$ci_upper, as.numeric(epitools_result2["uci"]), tolerance = 1e-6)

  # --- Birth Order 3 ---
  df3 <- prep_data_for_epiutils(count_matrix[, 3], population_matrix[, 3], standard_pop_epitools)
  our_result3 <- calculate_asr_direct(
    .df = df3,
    multiplier = 1,
    warn_small_cases = FALSE
  )
  epitools_result3 <- epitools::ageadjust.direct(
    count = count_matrix[, 3],
    pop = population_matrix[, 3],
    stdpop = standard_pop_epitools
  )
  expect_equal(our_result3$asr, as.numeric(epitools_result3["adj.rate"]), tolerance = 1e-6)
  expect_equal(our_result3$ci_lower, as.numeric(epitools_result3["lci"]), tolerance = 1e-6)
  expect_equal(our_result3$ci_upper, as.numeric(epitools_result3["uci"]), tolerance = 1e-6)

  # --- Birth Order 4 ---
  df4 <- prep_data_for_epiutils(count_matrix[, 4], population_matrix[, 4], standard_pop_epitools)
  our_result4 <- calculate_asr_direct(
    .df = df4,
    multiplier = 1,
    warn_small_cases = FALSE
  )
  epitools_result4 <- epitools::ageadjust.direct(
    count = count_matrix[, 4],
    pop = population_matrix[, 4],
    stdpop = standard_pop_epitools
  )
  expect_equal(our_result4$asr, as.numeric(epitools_result4["adj.rate"]), tolerance = 1e-6)
  expect_equal(our_result4$ci_lower, as.numeric(epitools_result4["lci"]), tolerance = 1e-6)
  expect_equal(our_result4$ci_upper, as.numeric(epitools_result4["uci"]), tolerance = 1e-6)

  # --- Birth Order 5+ ---
  df5p <- prep_data_for_epiutils(count_matrix[, 5], population_matrix[, 5], standard_pop_epitools)
  our_result5p <- calculate_asr_direct(
    .df = df5p,
    multiplier = 1,
    warn_small_cases = FALSE
  )
  epitools_result5p <- epitools::ageadjust.direct(
    count = count_matrix[, 5],
    pop = population_matrix[, 5],
    stdpop = standard_pop_epitools
  )
  expect_equal(our_result5p$asr, as.numeric(epitools_result5p["adj.rate"]), tolerance = 1e-6)
  expect_equal(our_result5p$ci_lower, as.numeric(epitools_result5p["lci"]), tolerance = 1e-6)
  expect_equal(our_result5p$ci_upper, as.numeric(epitools_result5p["uci"]), tolerance = 1e-6)

})

test_that("Total events and person-years columns are correctly calculated", {
  # Create simple test data
  test_data <- data.frame(
    age_group = c("0-19", "20-39", "40-59"),
    events = c(10L, 25L, 15L),
    person_years = c(1000, 2000, 1500),
    standard_pop = c(5000, 4000, 3000)
  )
  
  result <- calculate_asr_direct(.df = test_data, warn_small_cases = FALSE)
  
  # Test total_events
  expect_equal(result$total_events, 50L)  # 10 + 25 + 15
  
  # Test total_person_years  
  expect_equal(result$total_person_years, 4500)  # 1000 + 2000 + 1500
  
  # Verify these columns exist
  expect_true("total_events" %in% names(result))
  expect_true("total_person_years" %in% names(result))
  
  # Test that crude rate matches total_events/total_person_years
  expected_crude_rate <- 50 / 4500
  expect_equal(result$crude_rate, expected_crude_rate, tolerance = 1e-10)

})

test_that("Total columns handle edge cases correctly", {
  # Test with single age group
  single_age <- data.frame(
    age_group = "All ages",
    events = 100L,
    person_years = 10000,
    standard_pop = 50000
  )
  
  result_single <- calculate_asr_direct(.df = single_age, warn_small_cases = FALSE)
  expect_equal(result_single$total_events, 100L)
  expect_equal(result_single$total_person_years, 10000)
  
  # Test with zero events in some age groups
  zero_events <- data.frame(
    age_group = c("0-19", "20-39", "40+"),
    events = c(0L, 20L, 0L),
    person_years = c(1000, 2000, 500),
    standard_pop = c(5000, 4000, 2000)
  )
  
  result_zero <- calculate_asr_direct(.df = zero_events, warn_small_cases = FALSE)
  expect_equal(result_zero$total_events, 20L)
  expect_equal(result_zero$total_person_years, 3500)

})

test_that("calculate_asr_direct warnings are triggered correctly", {

  # Set up test data (same as main validation test)
  population_raw <- c(230061, 329449, 114920, 39487, 14208, 3052,
                      72202, 326701, 208667, 83228, 28466, 5375,
                      15050, 175702, 207081, 117300, 45026, 8660,
                      2293, 68800, 132424, 98301, 46075, 9834,
                      327, 30666, 123419, 149919, 104088, 34392,
                      319933, 931318, 786511, 488235, 237863, 61313)

  population_matrix <- matrix(population_raw, 6, 6,
                              dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                              c("1", "2", "3", "4", "5+", "Total")))

  count_raw <- c(107, 141, 60, 40, 39, 25,
                 25, 150, 110, 84, 82, 39,
                 3, 71, 114, 103, 108, 75,
                 1, 26, 64, 89, 137, 96,
                 0, 8, 63, 112, 262, 295,
                 136, 396, 411, 428, 628, 530)
  count_matrix <- matrix(count_raw, 6, 6,
                         dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                         c("1", "2", "3", "4", "5+", "Total")))

  standard_pop_epitools <- apply(population_matrix[, -6], 1, mean)

  prep_data_for_epiutils <- function(cases_vec, pop_vec, std_pop_vec) {
    data.frame(
      age_group = rownames(population_matrix)[1:6],
      events = as.integer(cases_vec),
      person_years = pop_vec,
      standard_pop = std_pop_vec
    )
  }

  # Test data from Birth Order 5+ which has zero cases in "Under 20" group
  df5p <- prep_data_for_epiutils(count_matrix[, 5], population_matrix[, 5], standard_pop_epitools)

  # Should warn about zero cases
  expect_warning(
    calculate_asr_direct(df5p, multiplier = 1, warn_small_cases = TRUE),
    "Found age groups with small/zero case counts"
  )

  # Test data from Birth Order 3 which has 3 cases in "Under 20" (small case count)
  df3 <- prep_data_for_epiutils(count_matrix[, 3], population_matrix[, 3], standard_pop_epitools)

  # Should warn about small case counts
  expect_warning(
    calculate_asr_direct(df3, multiplier = 1, warn_small_cases = TRUE),
    "Found age groups with small/zero case counts"
  )

  # Should include guidance message
  expect_warning(
    calculate_asr_direct(df3, multiplier = 1, warn_small_cases = TRUE),
    "Consider wider age groups"
  )

  # Test data from Birth Order 4 which has 1 case in "Under 20" (small case count)
  df4 <- prep_data_for_epiutils(count_matrix[, 4], population_matrix[, 4], standard_pop_epitools)

  # Should warn about small case counts
  expect_warning(
    calculate_asr_direct(df4, multiplier = 1, warn_small_cases = TRUE),
    "Found age groups with small/zero case counts"
  )

  # When warn_small_cases = FALSE, no warnings should be produced
  expect_silent(
    calculate_asr_direct(df5p, multiplier = 1, warn_small_cases = FALSE)
  )

  expect_silent(
    calculate_asr_direct(df3, multiplier = 1, warn_small_cases = FALSE)
  )

  # Results should be identical regardless of warning setting
  result_with_warnings <- suppressWarnings(calculate_asr_direct(df3, multiplier = 1, warn_small_cases = TRUE))
  result_without_warnings <- calculate_asr_direct(df3, multiplier = 1, warn_small_cases = FALSE)

  expect_equal(result_with_warnings$asr, result_without_warnings$asr)
  expect_equal(result_with_warnings$ci_lower, result_without_warnings$ci_lower)
  expect_equal(result_with_warnings$ci_upper, result_without_warnings$ci_upper)
})

# Tests comparing with PHEindicatormethods::calculate_dsr() ----
test_that("calculate_asr_direct() results match PHEindicatormethods::calculate_dsr()", {

  # Skip if PHEindicatormethods is not available
  skip_if_not_installed("PHEindicatormethods")

  # Set up the same test data as in the main epitools comparison test
  population_raw <- c(230061, 329449, 114920, 39487, 14208, 3052,
                      72202, 326701, 208667, 83228, 28466, 5375,
                      15050, 175702, 207081, 117300, 45026, 8660,
                      2293, 68800, 132424, 98301, 46075, 9834,
                      327, 30666, 123419, 149919, 104088, 34392,
                      319933, 931318, 786511, 488235, 237863, 61313)

  population_matrix <- matrix(population_raw, 6, 6,
                              dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                              c("1", "2", "3", "4", "5+", "Total")))

  count_raw <- c(107, 141, 60, 40, 39, 25,
                 25, 150, 110, 84, 82, 39,
                 3, 71, 114, 103, 108, 75,
                 1, 26, 64, 89, 137, 96,
                 0, 8, 63, 112, 262, 295,
                 136, 396, 411, 428, 628, 530)
  count_matrix <- matrix(count_raw, 6, 6,
                         dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                         c("1", "2", "3", "4", "5+", "Total")))

  # Use average population as standard
  standard_pop_epitools <- apply(population_matrix[, -6], 1, mean)

  # Test data setup - using Birth Order 1 data (good case counts)
  prep_data_for_epiutils <- function(cases_vec, pop_vec, std_pop_vec) {
    data.frame(
      age_group = rownames(population_matrix)[1:6],
      events = as.integer(cases_vec),
      person_years = pop_vec,
      standard_pop = std_pop_vec
    )
  }

  prep_data_for_phe <- function(cases_vec, pop_vec, std_pop_vec) {
    data.frame(
      x = as.integer(cases_vec),
      n = pop_vec,
      stdpop = std_pop_vec
    )
  }

  # Birth Order 1 data
  df_epi <- prep_data_for_epiutils(count_matrix[, 1], population_matrix[, 1], standard_pop_epitools)
  df_phe <- prep_data_for_phe(count_matrix[, 1], population_matrix[, 1], standard_pop_epitools)

  # Calculate using both methods (suppress warnings for comparison)
  result_epi <- calculate_asr_direct(df_epi, multiplier = 100000, warn_small_cases = FALSE)

  # PHE method (using the current calculate_dsr function)
  result_phe <- PHEindicatormethods::calculate_dsr(
    data = df_phe,
    x = x,
    n = n,
    stdpop = stdpop,
    type = "standard",
    confidence = 0.95,
    multiplier = 100000
  )

  # Compare point estimates (should be very close)
  expect_equal(result_epi$asr_scaled, result_phe$value, tolerance = 1e-6)

  # Compare confidence intervals (allow for methodological differences)
  # Our function uses gamma distribution method, PHE uses Byar's/Dobson method
  expect_equal(result_epi$ci_lower_scaled, result_phe$lowercl, tolerance = 0.5)
  expect_equal(result_epi$ci_upper_scaled, result_phe$uppercl, tolerance = 0.5)

  # Test with different multiplier
  result_epi_1k <- calculate_asr_direct(df_epi, multiplier = 1000, warn_small_cases = FALSE)
  result_phe_1k <- PHEindicatormethods::calculate_dsr(
    data = df_phe,
    x = x,
    n = n,
    stdpop = stdpop,
    type = "standard",
    confidence = 0.95,
    multiplier = 1000
  )

  expect_equal(result_epi_1k$asr_scaled, result_phe_1k$value, tolerance = 1e-6)
  expect_equal(result_epi_1k$ci_lower_scaled, result_phe_1k$lowercl, tolerance = 0.01)
  expect_equal(result_epi_1k$ci_upper_scaled, result_phe_1k$uppercl, tolerance = 0.01)
})

test_that("calculate_asr_direct() handles edge cases similarly to PHEindicatormethods", {

  # Skip if PHEindicatormethods is not available
  skip_if_not_installed("PHEindicatormethods")

  # Set up the same test data
  population_raw <- c(230061, 329449, 114920, 39487, 14208, 3052,
                      72202, 326701, 208667, 83228, 28466, 5375,
                      15050, 175702, 207081, 117300, 45026, 8660,
                      2293, 68800, 132424, 98301, 46075, 9834,
                      327, 30666, 123419, 149919, 104088, 34392,
                      319933, 931318, 786511, 488235, 237863, 61313)

  population_matrix <- matrix(population_raw, 6, 6,
                              dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                              c("1", "2", "3", "4", "5+", "Total")))

  count_raw <- c(107, 141, 60, 40, 39, 25,
                 25, 150, 110, 84, 82, 39,
                 3, 71, 114, 103, 108, 75,
                 1, 26, 64, 89, 137, 96,
                 0, 8, 63, 112, 262, 295,
                 136, 396, 411, 428, 628, 530)
  count_matrix <- matrix(count_raw, 6, 6,
                         dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40 and over"),
                                         c("1", "2", "3", "4", "5+", "Total")))

  # Use average population as standard
  standard_pop_epitools <- apply(population_matrix[, -6], 1, mean)

  # Test with small case counts (Birth Order 3)
  prep_data_for_epiutils <- function(cases_vec, pop_vec, std_pop_vec) {
    data.frame(
      age_group = rownames(population_matrix)[1:6],
      events = as.integer(cases_vec),
      person_years = pop_vec,
      standard_pop = std_pop_vec
    )
  }

  prep_data_for_phe <- function(cases_vec, pop_vec, std_pop_vec) {
    data.frame(
      x = as.integer(cases_vec),
      n = pop_vec,
      stdpop = std_pop_vec
    )
  }

  # Birth Order 3 data (has small case counts)
  df_epi <- prep_data_for_epiutils(count_matrix[, 3], population_matrix[, 3], standard_pop_epitools)
  df_phe <- prep_data_for_phe(count_matrix[, 3], population_matrix[, 3], standard_pop_epitools)

  # Calculate using both methods
  result_epi <- calculate_asr_direct(df_epi, multiplier = 100000, warn_small_cases = FALSE)
  result_phe <- PHEindicatormethods::calculate_dsr(
    data = df_phe,
    x = x,
    n = n,
    stdpop = stdpop,
    type = "standard",
    confidence = 0.95,
    multiplier = 100000
  )

  # Results should still match despite small case counts (allowing for method differences)
  expect_equal(result_epi$asr_scaled, result_phe$value, tolerance = 1e-6)
  expect_equal(result_epi$ci_lower_scaled, result_phe$lowercl, tolerance = 0.5)
  expect_equal(result_epi$ci_upper_scaled, result_phe$uppercl, tolerance = 0.5)

  # Test Birth Order 4 (even smaller case counts)
  df_epi_4 <- prep_data_for_epiutils(count_matrix[, 4], population_matrix[, 4], standard_pop_epitools)
  df_phe_4 <- prep_data_for_phe(count_matrix[, 4], population_matrix[, 4], standard_pop_epitools)

  result_epi_4 <- calculate_asr_direct(df_epi_4, multiplier = 100000, warn_small_cases = FALSE)
  result_phe_4 <- PHEindicatormethods::calculate_dsr(
    data = df_phe_4,
    x = x,
    n = n,
    stdpop = stdpop,
    type = "standard",
    confidence = 0.95,
    multiplier = 100000
  )

  expect_equal(result_epi_4$asr_scaled, result_phe_4$value, tolerance = 1e-6)
  expect_equal(result_epi_4$ci_lower_scaled, result_phe_4$lowercl, tolerance = 1.0)
  expect_equal(result_epi_4$ci_upper_scaled, result_phe_4$uppercl, tolerance = 8.5)
})

test_that("calculate_asr_direct() confidence intervals match PHE method", {

  # Skip if PHEindicatormethods is not available
  skip_if_not_installed("PHEindicatormethods")

  # Use a simple test case for precise comparison
  test_data_epi <- data.frame(
    age_group = c("0-19", "20-39", "40-59", "60+"),
    events = as.integer(c(5, 15, 35, 25)),
    person_years = c(10000, 8000, 6000, 4000),
    standard_pop = c(25000, 20000, 15000, 10000)
  )

  test_data_phe <- data.frame(
    x = as.integer(c(5, 15, 35, 25)),
    n = c(10000, 8000, 6000, 4000),
    stdpop = c(25000, 20000, 15000, 10000)
  )

  # Calculate using our method
  result_epi <- calculate_asr_direct(test_data_epi, multiplier = 100000, warn_small_cases = FALSE)

  # Calculate using PHE method (Byar's method with Dobson adjustment)
  result_phe <- PHEindicatormethods::calculate_dsr(
    data = test_data_phe,
    x = x,
    n = n,
    stdpop = stdpop,
    type = "standard",
    confidence = 0.95,
    multiplier = 100000
  )

  # Note: Our method uses gamma distribution, PHE uses Byar's/Dobson
  # Point estimates should match exactly
  expect_equal(result_epi$asr_scaled, result_phe$value, tolerance = 1e-6)

  # Confidence intervals may differ slightly due to different methods
  # but should be close for reasonable sample sizes
  expect_equal(result_epi$ci_lower_scaled, result_phe$lowercl, tolerance = 0.1)
  expect_equal(result_epi$ci_upper_scaled, result_phe$uppercl, tolerance = 0.1)

  # Test different confidence level
  result_epi_99 <- calculate_asr_direct(test_data_epi, multiplier = 100000,
                                       conf_level = 0.99, warn_small_cases = FALSE)
  result_phe_99 <- PHEindicatormethods::calculate_dsr(
    data = test_data_phe,
    x = x,
    n = n,
    stdpop = stdpop,
    type = "standard",
    confidence = 0.99,
    multiplier = 100000
  )

  expect_equal(result_epi_99$asr_scaled, result_phe_99$value, tolerance = 1e-6)
  expect_equal(result_epi_99$ci_lower_scaled, result_phe_99$lowercl, tolerance = 0.1)
  expect_equal(result_epi_99$ci_upper_scaled, result_phe_99$uppercl, tolerance = 0.1)
})

# Test warnings -----------------------------------------------------------

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
    "Found age groups with small/zero case counts"
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
    suppressWarnings(calculate_asr_direct(bad_data)),
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
    "Found age groups with small/zero case counts"
  )

  # Test multiple small cases warning
  expect_warning(
    calculate_asr_direct(multiple_small_cases),
    "Found age groups with small/zero case counts"
  )

  # Test informative guidance in warnings
  expect_warning(
    calculate_asr_direct(single_small_case),
    "Consider wider age groups"
  )
})


