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

  # Prepare data for your function
  # We need to test each 'birth order' column separately
  # Let's create a helper function to structure data for your function
  prep_data_for_my_func <- function(cases_vec, pop_vec) {
    data.frame(
      age_group = rownames(population_matrix)[1:6], # Use original age group names
      cases = cases_vec,
      population = pop_vec
    )
  }

  # --- Birth Order 1 ---
  df1 <- prep_data_for_my_func(count_matrix[, 1], population_matrix[, 1])
  my_result1 <- calculate_asr_direct(
    .df = df1,
    cases_col = "cases",
    population_col = "population",
    standard_pop = standard_pop_epitools,
    multiplier = 1 # Epitools returns raw rates, then multiplied later
  )
  epitools_result1 <- epitools::ageadjust.direct(
    count = count_matrix[, 1],
    pop = population_matrix[, 1],
    stdpop = standard_pop_epitools
  )

  # Check ASR (mean)
  expect_equal(my_result1$asr, epitools_result1[1, "rate"], tolerance = 1e-6)
  # Check lower CI
  expect_equal(my_result1$ci_lower, epitools_result1[1, "lower"], tolerance = 1e-6)
  # Check upper CI
  expect_equal(my_result1$ci_upper, epitools_result1[1, "upper"], tolerance = 1e-6)

  # --- Birth Order 2 ---
  df2 <- prep_data_for_my_func(count_matrix[, 2], population_matrix[, 2])
  my_result2 <- calculate_asr_direct(
    .df = df2,
    cases_col = "cases",
    population_col = "population",
    standard_pop = standard_pop_epitools,
    multiplier = 1
  )
  epitools_result2 <- epitools::ageadjust.direct(
    count = count_matrix[, 2],
    pop = population_matrix[, 2],
    stdpop = standard_pop_epitools
  )
  expect_equal(my_result2$asr, epitools_result2[1, "rate"], tolerance = 1e-6)
  expect_equal(my_result2$ci_lower, epitools_result2[1, "lower"], tolerance = 1e-6)
  expect_equal(my_result2$ci_upper, epitools_result2[1, "upper"], tolerance = 1e-6)

  # --- Birth Order 3 ---
  df3 <- prep_data_for_my_func(count_matrix[, 3], population_matrix[, 3])
  my_result3 <- calculate_asr_direct(
    .df = df3,
    cases_col = "cases",
    population_col = "population",
    standard_pop = standard_pop_epitools,
    multiplier = 1
  )
  epitools_result3 <- epitools::ageadjust.direct(
    count = count_matrix[, 3],
    pop = population_matrix[, 3],
    stdpop = standard_pop_epitools
  )
  expect_equal(my_result3$asr, epitools_result3[1, "rate"], tolerance = 1e-6)
  expect_equal(my_result3$ci_lower, epitools_result3[1, "lower"], tolerance = 1e-6)
  expect_equal(my_result3$ci_upper, epitools_result3[1, "upper"], tolerance = 1e-6)

  # --- Birth Order 4 ---
  df4 <- prep_data_for_my_func(count_matrix[, 4], population_matrix[, 4])
  my_result4 <- calculate_asr_direct(
    .df = df4,
    cases_col = "cases",
    population_col = "population",
    standard_pop = standard_pop_epitools,
    multiplier = 1
  )
  epitools_result4 <- epitools::ageadjust.direct(
    count = count_matrix[, 4],
    pop = population_matrix[, 4],
    stdpop = standard_pop_epitools
  )
  expect_equal(my_result4$asr, epitools_result4[1, "rate"], tolerance = 1e-6)
  expect_equal(my_result4$ci_lower, epitools_result4[1, "lower"], tolerance = 1e-6)
  expect_equal(my_result4$ci_upper, epitools_result4[1, "upper"], tolerance = 1e-6)

  # --- Birth Order 5+ ---
  df5p <- prep_data_for_my_func(count_matrix[, 5], population_matrix[, 5])
  my_result5p <- calculate_asr_direct(
    .df = df5p,
    cases_col = "cases",
    population_col = "population",
    standard_pop = standard_pop_epitools,
    multiplier = 1
  )
  epitools_result5p <- epitools::ageadjust.direct(
    count = count_matrix[, 5],
    pop = population_matrix[, 5],
    stdpop = standard_pop_epitools
  )
  expect_equal(my_result5p$asr, epitools_result5p[1, "rate"], tolerance = 1e-6)
  expect_equal(my_result5p$ci_lower, epitools_result5p[1, "lower"], tolerance = 1e-6)
  expect_equal(my_result5p$ci_upper, epitools_result5p[1, "upper"], tolerance = 1e-6)

})
