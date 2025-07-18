# install.packages("survival")
library(survival)

# Re-using individual_data from Method 1
# Ensure dob, study_entry_date, study_exit_date, event_date are Date objects
individual_data <- tibble(
  patient_id = c(1, 2, 3, 4, 5),
  disease_type = c("Stroke", "Stroke", "MI", "Stroke", "CKD"),
  dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01", "1940-03-10", "1988-11-25")),
  study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01", "2022-05-01", "2020-01-01")),
  study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10", "2023-07-20", "2023-12-31")), # End of observation or event if no event_date
  event_date = as.Date(c("2023-10-01", NA, "2022-04-10", NA, NA)) # NA if no event during FU
)

# Define age cuts (breakpoints)
# These are the *start* of each age interval.
# Make sure these match your age_breaks from Method 1 to align with standard pop.
age_cut_points <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100) # Ends at Inf

# Prepare data for pyears
pyears_data <- individual_data %>%
  # Calculate age at study entry
  mutate(age_at_entry = as.numeric(difftime(study_entry_date, dob, units = "days") / 365.25)) %>%
  # Create a Surv object for follow-up time and event status
  # event_date should be NA for censored, date for event
  # event_status is 1 for event, 0 for censored
  mutate(event_status = as.integer(!is.na(event_date))) %>%
  # The time of event/censoring relative to study_entry_date
  mutate(futime = lubridate::time_length(dplyr::if_else(is.na(event_date), study_exit_date, event_date) - study_entry_date, unit = "year"))

# Remove rows with negative or zero follow-up time
pyears_data <- pyears_data %>% filter(futime > 0)


# Calculate person-years using pyears()
# This function automatically handles age transitions
person_years_results <- pyears(
  formula = Surv(futime, event_status) ~ tcut(age_at_entry, age_cut_points),
  data = pyears_data,
  scale = 1 # Already in years
)

# Extract and format the results
# pyears returns an array, convert to data.frame
person_years_summary_df <- person_years_results$pyears |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "age_group") |>
  as_tibble() %>%
  # Clean up age group labels to match your desired format (e.g., "60-65" -> "60-64")
  # This step might require careful string manipulation depending on pyears' output labels
  mutate("age_group" = paste0("[", stringr::str_replace(stringr::str_trim(.data[["age_group"]]), "\\+\\s+thru\\s+", "-"), ")")) |>
  dplyr::select(
    age_group,
    "person_years" = `person_years_results$pyears`
  ) |>
  # Convert age_group back to factor with correct levels for consistency
  mutate(age_group = factor(age_group, levels = age_group))
