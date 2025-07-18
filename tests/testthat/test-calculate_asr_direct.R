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
    multiplier = 1 # Epitools returns raw rates, then multiplied later
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
    multiplier = 1
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
    multiplier = 1
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
    multiplier = 1
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
    multiplier = 1
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
