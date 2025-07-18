#' Calculate Age-Standardised Incidence Rates with Confidence Intervals
#'
#' This function calculates age-standardised incidence rates (ASR) using the direct
#' standardisation method with gamma distribution-based confidence intervals.
#' The gamma method is preferred as it naturally prevents negative confidence
#' interval bounds and is the standard approach in epidemiological literature.
#'
#' @param .df A data frame containing age-specific case counts and population data
#' @param cases_col Character string specifying the column name for case counts
#' @param population_col Character string specifying the column name for population sizes
#' @param standard_pop Numeric vector of standard population weights for each age group
#' @param conf_level Confidence level for confidence intervals (default: 0.95)
#' @param multiplier Multiplier for rate expression (default: 100000 for rates per 100,000)
#'
#' @return A tibble containing:
#' - **crude_rate**: Crude incidence rate
#' - **crude_rate_scaled**: Crude rate multiplied by multiplier
#' - **asr**: Age-standardised incidence rate
#' - **asr_scaled**: ASR multiplied by multiplier
#' - **ci_lower**: Lower confidence interval bound for ASR
#' - **ci_upper**: Upper confidence interval bound for ASR
#' - **ci_lower_scaled**: Lower CI multiplied by multiplier
#' - **ci_upper_scaled**: Upper CI multiplied by multiplier
#' - **conf_level**: Confidence level used
#' - **method**: Method used for CI calculation
#' - **age_specific_data**: Data frame with age-specific rates and details
#'
#' @details
#' The function uses the gamma distribution method for confidence interval calculation,
#' which is the standard approach in epidemiological software like epitools.
#' This method naturally prevents negative confidence interval bounds and is
#' appropriate for rate data.
#'
#' The direct standardisation method calculates ASR as:
#' ASR = Σ(w_i × r_i) where w_i are standardised weights and r_i are age-specific rates
#'
#' @examples
#' # Example data
#' pop_data <- data.frame(
#'   age_group = c("0-19", "20-39", "40-59", "60-79", "80+"),
#'   cases = c(5, 25, 150, 300, 80),
#'   population = c(20000, 25000, 22000, 15000, 3000)
#' )
#'
#' # Standard population (World Standard Population)
#' std_pop <- c(35000, 25000, 20000, 15000, 5000)
#'
#' # Calculate ASR
#' result <- calculate_asr_direct(
#'   .df = pop_data,
#'   cases_col = "cases",
#'   population_col = "population",
#'   standard_pop = std_pop
#' )
#'
#' # View results
#' print(result$asr_scaled)  # ASR per 100,000
#' print(result$ci_lower_scaled)  # Lower CI per 100,000
#' print(result$ci_upper_scaled)  # Upper CI per 100,000
#'
#' @references
#' Breslow, N. E., & Day, N. E. (1987). Statistical methods in cancer research.
#' Volume II--The design and analysis of cohort studies. IARC scientific publications, (82), 1-406.
#'
#' @importFrom dplyr mutate select pull everything all_of
#' @importFrom tibble tibble
#' @importFrom stats qgamma
#' @export
calculate_asr_direct <- function(.df,
                                 cases_col,
                                 population_col,
                                 standard_pop,
                                 conf_level = 0.95,
                                 multiplier = 100000) {
  # Input validation
  if (!is.data.frame(.df)) {
    cli::cli_abort(c(
      "x" = ".df must be a data frame",
      "i" = "You provided a {.cls {class(.df)}}"
    ))
  }

  if (!cases_col %in% names(.df)) {
    cli::cli_abort(c(
      "x" = "Column {.val {cases_col}} not found in data",
      "i" = "Available columns: {.val {names(.df)}}"
    ))
  }

  if (!population_col %in% names(.df)) {
    cli::cli_abort(c(
      "x" = "Column {.val {population_col}} not found in data",
      "i" = "Available columns: {.val {names(.df)}}"
    ))
  }

  if (nrow(.df) != length(standard_pop)) {
    cli::cli_abort(c(
      "x" = "Length of standard_pop must match number of rows in data",
      "i" = "Data has {nrow(.df)} row{?s}, but standard_pop has {length(standard_pop)} value{?s}"
    ))
  }

  if (conf_level <= 0 || conf_level >= 1) {
    cli::cli_abort(c(
      "x" = "conf_level must be between 0 and 1",
      "i" = "You provided {.val {conf_level}}"
    ))
  }

  if (multiplier <= 0) {
    cli::cli_abort(c(
      "x" = "multiplier must be positive",
      "i" = "You provided {.val {multiplier}}"
    ))
  }

  # Calculate age-specific rates and weights using tidyverse
  asr_data <- .df |>
    dplyr::mutate(
      "cases" = .data[[cases_col]],
      "population" = .data[[population_col]],
      "standard_pop" = standard_pop,
      "age_specific_rate" = .data[["cases"]] / .data[["population"]],
      "std_weight" = .data[["standard_pop"]] / sum(.data[["standard_pop"]]),
      "expected_cases" = .data[["age_specific_rate"]] * .data[["standard_pop"]],
      .keep = "all"
    )

  # Calculate summary statistics
  total_cases <- sum(asr_data$cases)
  total_population <- sum(asr_data$population)
  total_std_pop <- sum(asr_data$standard_pop)

  # Calculate rates
  crude_rate <- total_cases / total_population
  asr <- sum(asr_data$expected_cases) / total_std_pop

  # Calculate confidence intervals using gamma method
  alpha <- 1 - conf_level

  # Variance calculation for gamma method
  variance_components <- asr_data |>
    dplyr::mutate("variance_term" = (.data[["std_weight"]]^2) * (.data[["cases"]] / .data[["population"]]^2)) |>
    dplyr::pull("variance_term")

  dsr_var <- sum(variance_components)

  # Calculate maximum weight term for boundary adjustment
  wm_components <- asr_data |>
    dplyr::mutate("wm_term" = .data[["std_weight"]] / .data[["population"]]) |>
    dplyr::pull("wm_term")

  wm <- max(wm_components)

  # Gamma distribution parameters for lower CI
  shape_param_lower <- (asr^2) / dsr_var
  scale_param_lower <- dsr_var / asr

  # Gamma distribution parameters for upper CI (with boundary adjustment)
  shape_param_upper <- ((asr + wm)^2) / (dsr_var + wm^2)
  scale_param_upper <- (dsr_var + wm^2) / (asr + wm)

  # Calculate confidence intervals
  ci_lower <- stats::qgamma(alpha / 2, shape = shape_param_lower, scale = scale_param_lower)
  ci_upper <- stats::qgamma(1 - alpha / 2, shape = shape_param_upper, scale = scale_param_upper)

  # Prepare detailed age-specific results
  age_specific_results <- asr_data |>
    dplyr::mutate(
      "age_specific_rate_scaled" = .data[["age_specific_rate"]] * !!multiplier,
      "contribution_to_asr" = .data[["age_specific_rate"]] * .data[["std_weight"]],
      .keep = "all"
    ) |>
    dplyr::select(dplyr::everything(), -dplyr::all_of("expected_cases"))

  # Return comprehensive results
  tibble::tibble(
    crude_rate = crude_rate,
    crude_rate_scaled = crude_rate * multiplier,
    asr = asr,
    asr_scaled = asr * multiplier,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    ci_lower_scaled = ci_lower * multiplier,
    ci_upper_scaled = ci_upper * multiplier,
    conf_level = conf_level,
    age_specific_data = list(age_specific_results)
  )
}
