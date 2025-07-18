#' World Health Organisation 2000-2025 World Standard Population
#'
#' The Standard in SEER*Stat for the 90-94 age group needed to be increased by
#' one so the total would sum to 1,000,000. That group was selected because it
#' was the number closest to rounding up that did not.
#'
#' @format ## `who_2000_2025_standard_population`
#' \describe{
#'   \item{age_group}{Age Group}
#'   \item{who_world_standard_percent}{WHO World Standard (%)}
#'   \item{recalculation_to_add_to_1million}{Recalculation to add to 1,000,000}
#'   \item{rounded_to_integers}{Rounded to Integers}
#'   \item{standard_for_seer_stat}{Standard For SEER*Stat}
#'   \item{lower_age_limit}{Lower limit for age group}
#' }
#'
#' @source <https://seer.cancer.gov/stdpopulations/world.who.html>
"who_2000_2025_standard_population"
