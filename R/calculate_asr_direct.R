#' Calculate Age-Standardised Rates with Confidence Intervals
#'
#' This function calculates age-standardised rates using the direct 
#' standardisation method with confidence intervals. It can be used for both 
#' incidence rates (events over time) and prevalence rates (cross-sectional 
#' data). The gamma method is preferred for confidence intervals as it naturally 
#' prevents negative bounds and is the standard approach in epidemiological 
#' literature.
#'
#' @param .df A data frame containing age-specific case counts, population data,
#'   and standard population weights. Must contain the following columns:
#'   - **age_group**: Age group labels
#'   - **events**: Number of events/cases in each age group (integer). For incidence: 
#'     new cases over time. For prevalence: individuals with condition at a point in time.
#'   - **person_years**: Person-years of follow-up (incidence) or population size 
#'     (prevalence) in each age group (numeric). For prevalence studies, use the 
#'     total number of individuals examined/surveyed.
#'   - **standard_pop**: Standard population weights for each age group (numeric)
#' @param conf_level Confidence level for confidence intervals (default: 0.95)
#' @param multiplier Multiplier for rate expression (default: 100000 for rates
#'   per 100,000). Use 100 for prevalence studies to express results as percentages.
#' @param ci_method Character string specifying the confidence interval
#'   calculation method. Options are "gamma" (default) or "byars". The gamma
#'   method uses the gamma distribution approach (consistent with
#'   [epitools](https://CRAN.R-project.org/package=epitools)), while the Byar's
#'   method uses Byar's approximation with Dobson adjustment (consistent with
#'   [PHEindicatormethods](https://CRAN.R-project.org/package=PHEindicatormethods)).
#'   Both methods are statistically valid; differences are typically minimal.
#' @param warn_small_cases Logical. If TRUE (default), warns when age groups
#'   have zero or < 5 cases which may lead to unstable rate estimates, and warns 
#'   when age groups with zero person-years are automatically excluded. Set to 
#'   FALSE to suppress these data quality warnings. Consider wider age groups 
#'   for more stable results.
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
#' - **total_events**: Total number of events across all age groups
#' - **total_person_years**: Total person-years across all age groups
#' - **age_specific_data**: Data frame with age-specific rates and details
#'
#' @details The function uses the gamma distribution method for confidence
#'   interval calculation, which is the standard approach in epidemiological
#'   software like epitools. This method naturally prevents negative confidence
#'   interval bounds and is appropriate for rate data.
#'
#'   The direct standardisation method calculates ASR as: ASR = Σ(w_i × r_i)
#'   where w_i are standardised weights and r_i are age-specific rates
#'
#' **Incidence vs Prevalence:**
#' - **Incidence rates**: Use person-years of follow-up in denominator, typically expressed per 100,000
#' - **Prevalence rates**: Use total individuals examined in denominator, typically expressed as percentages (set multiplier = 100)
#'
#' **Age Grouping Considerations:**
#' - Age groups with zero person-years are automatically excluded from calculation
#' - Zero cases are allowed but may indicate very low incidence or small populations
#' - Age groups with < 5 cases produce less stable rate estimates and wider confidence intervals
#' - Consider wider age groups if many strata have very few cases
#' - The function balances statistical stability with age-specific precision
#' - Use `warn_small_cases = FALSE` to suppress data quality warnings
#'
#' @examples
#' # Example 1: Incidence rates (per 100,000)
#' incidence_data <- data.frame(
#'   age_group = c("0-19", "20-39", "40-59", "60-79", "80+"),
#'   events = c(5L, 25L, 150L, 300L, 80L),
#'   person_years = c(20000, 25000, 22000, 15000, 3000),
#'   standard_pop = c(35000, 25000, 20000, 15000, 5000)
#' )
#'
#' # Calculate age-standardised incidence rate
#' incidence_result <- calculate_asr_direct(.df = incidence_data)
#' print(incidence_result$asr_scaled)  # ASR per 100,000
#'
#' # Example 2: Prevalence rates (as percentages)
#' prevalence_data <- data.frame(
#'   age_group = c("18-29", "30-49", "50-69", "70+"),
#'   events = c(12L, 45L, 98L, 67L),  # Individuals with condition
#'   person_years = c(1200, 1500, 1400, 800),  # Individuals surveyed
#'   standard_pop = c(2435, 3847, 2603, 763)
#' )
#'
#' # Calculate age-standardised prevalence (use multiplier = 100 for percentages)
#' prevalence_result <- calculate_asr_direct(.df = prevalence_data, multiplier = 100)
#' print(paste("Prevalence:", round(prevalence_result$asr_scaled, 2), "%"))
#'
#' @references Breslow, N. E., & Day, N. E. (1987). Statistical methods in
#'   cancer research. Volume II--The design and analysis of cohort studies. IARC
#'   scientific publications, (82), 1-406.
#'
#' @importFrom dplyr mutate select pull everything all_of
#' @importFrom tibble tibble
#' @importFrom stats qgamma qchisq
#' @export
calculate_asr_direct <- function(.df,
                                 conf_level = 0.95,
                                 multiplier = 100000,
                                 ci_method = "gamma",
                                 warn_small_cases = TRUE) {
  # Input validation
  if (!is.data.frame(.df)) {
    cli::cli_abort(c(
      "x" = ".df must be a data frame",
      "i" = "You provided a {.cls {class(.df)}}"
    ))
  }

  # Check required columns
  required_cols <- c("age_group", "events", "person_years", "standard_pop")
  missing_cols <- setdiff(required_cols, names(.df))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Missing required column{?s}: {.val {missing_cols}}",
      "i" = "Available columns: {.val {names(.df)}}",
      "i" = "Required columns: age_group, events, person_years, standard_pop"
    ))
  }

  # Check for missing values
  has_missing <- sapply(.df[required_cols], \(.x) any(is.na(.x)))
  if (any(has_missing)) {
    missing_cols <- names(has_missing)[has_missing]
    cli::cli_abort(c(
      "x" = "Missing values found in column{?s}: {.val {missing_cols}}",
      "i" = "All required columns must have complete data"
    ))
  }

  # Check data types
  if (!is.integer(.df$events)) {
    cli::cli_abort(c(
      "x" = "Column 'events' must be of type integer",
      "i" = "Currently has type: {.cls {typeof(.df$events)}}"
    ))
  }

  if (!is.numeric(.df$person_years)) {
    cli::cli_abort(c(
      "x" = "Column 'person_years' must be numeric",
      "i" = "Currently has type: {.cls {typeof(.df$person_years)}}"
    ))
  }

  if (!is.numeric(.df$standard_pop)) {
    cli::cli_abort(c(
      "x" = "Column 'standard_pop' must be numeric",
      "i" = "Currently has type: {.cls {typeof(.df$standard_pop)}}"
    ))
  }

  # Validate CI method
  ci_method <- rlang::arg_match(ci_method,
                                values = c("gamma", "byars"))

  # Check for non-negative values
  if (any(.df$events < 0)) {
    cli::cli_abort(c(
      "x" = "Column 'events' must contain non-negative values",
      "i" = "Found negative values"
    ))
  }

  if (any(.df$person_years < 0)) {
    cli::cli_abort(c(
      "x" = "Column 'person_years' must contain non-negative values",
      "i" = "Found negative values"
    ))
  }

  # Handle zero person-years groups - exclude them automatically
  zero_py_groups <- .df$person_years == 0
  if (any(zero_py_groups)) {
    zero_py_ages <- .df$age_group[zero_py_groups]
    n_excluded <- sum(zero_py_groups)
    
    if (warn_small_cases) {
      cli::cli_warn(c(
        "!" = "Excluding {n_excluded} age group{?s} with zero person-years:",
        "*" = "{paste(zero_py_ages, collapse = ', ')}",
        "i" = "Age-standardised rates can only be calculated for groups with positive person-years"
      ))
    }
    
    # Filter out zero person-years groups
    .df <- .df[!zero_py_groups, , drop = FALSE]
    
    # Check if any data remains
    if (nrow(.df) == 0) {
      cli::cli_abort(c(
        "x" = "No age groups remain after excluding those with zero person-years",
        "i" = "All age groups had zero person-years"
      ))
    }
  }

  # Warn about small and zero case counts (but allow them)
  if (warn_small_cases) {
    small_case_groups <- .df$events < 5 & .df$events > 0
    zero_case_groups <- .df$events == 0

    if (any(small_case_groups) || any(zero_case_groups)) {
      warning_parts <- c()

      if (any(zero_case_groups)) {
        zero_ages <- .df$age_group[zero_case_groups]
        n_zero <- sum(zero_case_groups)
        warning_parts <- c(warning_parts,
                          paste0("zero cases: ", paste(zero_ages, collapse = ", ")))
      }

      if (any(small_case_groups)) {
        small_ages <- .df$age_group[small_case_groups]
        small_counts <- .df$events[small_case_groups]
        small_info <- paste(small_ages, " (", small_counts, " case", ifelse(small_counts == 1, "", "s"), ")", sep = "")
        warning_parts <- c(warning_parts,
                          paste0("< 5 cases: ", paste(small_info, collapse = ", ")))
      }

      cli::cli_warn(c(
        "!" = "Found age groups with small/zero case counts:",
        "*" = "{warning_parts}",
        "i" = "Small and zero case counts may produce unstable rate estimates",
        "i" = "Consider wider age groups, longer follow-up, or data quality checks"
      ))
    }
  }

  if (any(.df$standard_pop <= 0)) {
    cli::cli_abort(c(
      "x" = "Column 'standard_pop' must contain positive values",
      "i" = "Found non-positive values"
    ))
  }

  # Final check: ensure remaining person-years are positive
  if (any(.df$person_years <= 0)) {
    cli::cli_abort(c(
      "x" = "Internal error: person_years should be positive after filtering",
      "i" = "This should not happen - please report this bug"
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

  # Calculate age-specific rates and weights
  asr_data <- .df |>
    dplyr::mutate(
      "age_specific_rate" = .data[["events"]] / .data[["person_years"]],
      "std_weight" = .data[["standard_pop"]] / sum(.data[["standard_pop"]]),
      "expected_cases" = .data[["age_specific_rate"]] * .data[["standard_pop"]],
      .keep = "all"
    )

  # Calculate summary statistics
  total_cases <- sum(asr_data$events)
  total_population <- sum(asr_data$person_years)
  total_std_pop <- sum(asr_data$standard_pop)

  # Calculate rates
  crude_rate <- total_cases / total_population
  asr <- sum(asr_data$expected_cases) / total_std_pop

  # Calculate confidence intervals using gamma method
  alpha <- 1 - conf_level

  # Variance calculation for gamma method
  variance_components <- asr_data |>
    dplyr::mutate("variance_term" = (.data[["std_weight"]]^2) * (.data[["events"]] / .data[["person_years"]]^2)) |>
    dplyr::pull("variance_term")

  dsr_var <- sum(variance_components)

  # Calculate confidence intervals using the selected method
  if (ci_method == "gamma") {
    # Gamma distribution method (consistent with epitools)

    # Calculate maximum weight term for boundary adjustment
    wm_components <- asr_data |>
      dplyr::mutate("wm_term" = .data[["std_weight"]] / .data[["person_years"]]) |>
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

  } else if (ci_method == "byars") {
    # Byar's method with Dobson adjustment (consistent with PHEindicatormethods)

    # Calculate total cases for Byar's approximation
    total_cases <- sum(asr_data$events)

    if (total_cases < 10) {
      # For very small counts, use a conservative approach
      # Calculate CIs based on total cases using exact Poisson
      byars_lower <- ifelse(total_cases == 0, 0,
                           stats::qchisq(alpha / 2, 2 * total_cases) / 2)
      byars_upper <- stats::qchisq(1 - alpha / 2, 2 * (total_cases + 1)) / 2

      # Scale by standard population ratio
      ci_lower <- (byars_lower / total_population) * (total_std_pop / total_std_pop) * asr
      ci_upper <- (byars_upper / total_population) * (total_std_pop / total_std_pop) * asr

    } else {
      # Standard Byar's approximation for larger counts
      # Use normal approximation with continuity correction
      z_value <- stats::qnorm(1 - alpha / 2)

      # Byar's continuity correction
      byars_se <- sqrt(dsr_var)

      # Apply Dobson adjustment for weighted sums
      dobson_factor <- sqrt(1 + (dsr_var / (asr^2)))
      adjusted_se <- byars_se * dobson_factor

      ci_lower <- asr - z_value * adjusted_se
      ci_upper <- asr + z_value * adjusted_se

      # Ensure lower bound is non-negative
      ci_lower <- max(0, ci_lower)
    }
  }

  # Prepare detailed age-specific results
  age_specific_results <- asr_data |>
    dplyr::mutate(
      "age_specific_rate_scaled" = .data[["age_specific_rate"]] * !!multiplier,
      "contribution_to_asr" = .data[["age_specific_rate"]] * .data[["std_weight"]],
      .keep = "all"
    ) |>
    dplyr::select(dplyr::everything(), -dplyr::all_of("expected_cases"))

  # Calculate total events and person-years
  total_events <- sum(asr_data[["events"]], na.rm = TRUE)
  total_person_years <- sum(asr_data[["person_years"]], na.rm = TRUE)

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
    total_events = total_events,
    total_person_years = total_person_years,
    age_specific_data = list(age_specific_results)
  )
}
