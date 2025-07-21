#' Calculate Person-Years by Age Strata
#'
#' This function calculates person-years of follow-up by age strata for cohort studies.
#' It handles age transitions during follow-up by splitting person-time contributions
#' across age groups as individuals age during the observation period.
#'
#' @param .df A data frame containing individual-level follow-up data
#' @param age_cut_points Numeric vector of age breakpoints defining the age strata.
#'   Default is 5-year age groups from 0 to 100+: `c(0, 5, 10, ..., 95, 100, 150)`.
#'   This matches the WHO 2000-2025 Standard Population age groups. The final 
#'   age group is displayed as "[100+)" regardless of the upper cut point value.
#'
#' @details
#' The function requires the following columns in `.df`:
#' - **patient_id**: Unique identifier for each individual
#' - **dob**: Date of birth (Date class)
#' - **study_entry_date**: Date of study entry (Date class)
#' - **study_exit_date**: Date of study exit/end of follow-up (Date class)
#' - **event_status**: Event indicator (integer: 1 = event occurred, 0 = censored)
#'
#' The function automatically handles individuals who transition between age groups
#' during follow-up by splitting their person-time contributions appropriately.
#'
#' @return A tibble containing:
#' - **age_group**: Age group labels (factor)
#' - **person_years**: Total person-years of follow-up in each age group
#' - **events**: Number of events in each age group
#'
#' @examples
#' # Example data
#' cohort_data <- data.frame(
#'   patient_id = 1:3,
#'   dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
#'   study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
#'   study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
#'   event_status = c(1L, 0L, 1L)
#' )
#'
#' # Calculate person-years by 5-year age groups
#' result <- calculate_person_years_by_age_strata(cohort_data)
#'
#' # Custom age groups
#' custom_ages <- c(0, 20, 40, 60, 80, 100)
#' result_custom <- calculate_person_years_by_age_strata(cohort_data, custom_ages)
#'
#' @export
calculate_person_years_by_age_strata <- function(.df,
                                                     age_cut_points = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 150)) {

  # Input validation
  if (!is.data.frame(.df)) {
    cli::cli_abort(c(
      "x" = ".df must be a data frame",
      "i" = "You provided a {.cls {class(.df)}}"
    ))
  }

  # Check required columns
  required_cols <- c("patient_id", "dob", "study_entry_date", "study_exit_date", "event_status")
  missing_cols <- setdiff(required_cols, names(.df))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Missing required column{?s}: {.val {missing_cols}}",
      "i" = "Available columns: {.val {names(.df)}}"
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

  # Check for unique patient_id
  if (any(duplicated(.df$patient_id))) {
    n_duplicates <- sum(duplicated(.df$patient_id))
    cli::cli_abort(c(
      "x" = "Duplicate patient_id values found",
      "i" = "Found {n_duplicates} duplicate{?s}",
      "i" = "Each patient_id must appear only once"
    ))
  }

  # Check date columns are Date class
  date_cols <- c("dob", "study_entry_date", "study_exit_date")
  for (col in date_cols) {
    if (!inherits(.df[[col]], "Date")) {
      cli::cli_abort(c(
        "x" = "Column {.val {col}} must be of class Date",
        "i" = "Currently has class: {.cls {class(.df[[col]])}}"
      ))
    }
  }

  # Check event_status is integer with values 0 or 1
  if (!is.integer(.df$event_status)) {
    cli::cli_abort(c(
      "x" = "Column event_status must be of type integer",
      "i" = "Currently has type: {.cls {typeof(.df$event_status)}}"
    ))
  }

  valid_event_values <- all(.df$event_status %in% c(0L, 1L))
  if (!valid_event_values) {
    invalid_values <- unique(.df$event_status[!.df$event_status %in% c(0L, 1L)])
    cli::cli_abort(c(
      "x" = "event_status must contain only 0 (censored) or 1 (event)",
      "i" = "Found invalid value{?s}: {.val {invalid_values}}"
    ))
  }

  # Validate age_cut_points
  if (!is.numeric(age_cut_points) || length(age_cut_points) < 2) {
    cli::cli_abort(c(
      "x" = "age_cut_points must be a numeric vector with at least 2 values",
      "i" = "You provided: {.val {age_cut_points}}"
    ))
  }

  if (is.unsorted(age_cut_points)) {
    cli::cli_abort(c(
      "x" = "age_cut_points must be in ascending order",
      "i" = "You provided: {.val {age_cut_points}}"
    ))
  }

  # Prepare data for pyears
  pyears_data <- .df |>
    dplyr::mutate(
      "age_at_entry" = as.numeric(difftime(.data[["study_entry_date"]], .data[["dob"]], units = "days") / 365.25),
      "futime" = lubridate::time_length(.data[["study_exit_date"]] - .data[["study_entry_date"]], unit = "year")
    )

  # Check for negative or zero follow-up time
  invalid_futime <- pyears_data$futime <= 0
  if (any(invalid_futime)) {
    n_invalid <- sum(invalid_futime)
    cli::cli_abort(c(
      "x" = "Invalid follow-up time found",
      "i" = "Found {n_invalid} patient{?s} with follow-up time <= 0",
      "i" = "study_exit_date must be after study_entry_date"
    ))
  }

  # Check if any patients will be excluded due to age cut points
  min_age <- min(pyears_data$age_at_entry)
  max_age <- max(pyears_data$age_at_entry)
  min_cut <- min(age_cut_points)
  max_cut <- max(age_cut_points)

  excluded_below <- pyears_data$age_at_entry < min_cut
  excluded_above <- pyears_data$age_at_entry >= max_cut

  if (any(excluded_below) || any(excluded_above)) {
    n_excluded_below <- sum(excluded_below)
    n_excluded_above <- sum(excluded_above)
    total_excluded <- n_excluded_below + n_excluded_above

    cli::cli_warn(c(
      "!" = "Some patients will be excluded due to age cut points",
      "i" = "Age range in data: {round(min_age, 1)} to {round(max_age, 1)} years",
      "i" = "Age cut points range: {min_cut} to {max_cut} years",
      "i" = "Patients excluded (too young): {n_excluded_below}",
      "i" = "Patients excluded (too old): {n_excluded_above}",
      "i" = "Total excluded: {total_excluded} of {nrow(.df)}"
    ))
  }

  # Calculate person-years using pyears()
  person_years_results <- survival::pyears(
    formula = survival::Surv(futime, event_status) ~ survival::tcut(age_at_entry, age_cut_points),
    data = pyears_data,
    scale = 1 # Already in years
  )

  # Extract and format the results
  result <- tibble::tibble(
    age_group = factor(names(person_years_results$pyears)),
    person_years = as.numeric(person_years_results$pyears),
    events = as.integer(person_years_results$event),
    n = as.integer(person_years_results$n)
  )

  # Convert age_group to factor for consistent ordering
  result <- result |>
    dplyr::mutate("age_group" = paste0(
      "[",
      stringr::str_replace(stringr::str_trim(.data[["age_group"]]), "\\+\\s+thru\\s+", "-"),
      ")"
    )) |>
    # Convert the final age group to match WHO format [100+)
    dplyr::mutate("age_group" = stringr::str_replace(.data[["age_group"]], "\\[100-\\d+\\)", "[100+)")) |>
    dplyr::mutate("age_group" = factor(.data[["age_group"]], levels = unique(.data[["age_group"]])))

  return(result)
}
