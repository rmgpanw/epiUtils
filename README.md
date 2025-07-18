
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epiUtils

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/rmgpanw/epiUtils/graph/badge.svg)](https://app.codecov.io/gh/rmgpanw/epiUtils)
[![R-CMD-check](https://github.com/rmgpanw/epiUtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmgpanw/epiUtils/actions/workflows/R-CMD-check.yaml)
[![pkgdown.yaml](https://github.com/rmgpanw/epiUtils/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/rmgpanw/epiUtils/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

`epiUtils` provides essential tools for epidemiological analysis,
including age-standardised rate calculations and person-years
computation for cohort studies. The package implements established
epidemiological methods with modern R practices, comprehensive
validation, and built-in standard population data.

## Key Features

- **Age-Standardised Rates**: Calculate directly standardised rates with
  gamma or Byar’s confidence intervals
- **Person-Years Calculation**: Compute person-years by age strata with
  automatic age-transition handling
- **WHO Standard Population**: Built-in WHO 2000-2025 World Standard
  Population data

## Installation

You can install the development version of epiUtils from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("alasdair/epiUtils")
```

## Quick Start

### Age-Standardised Rates

Calculate age-standardised incidence rates using the direct
standardisation method:

``` r
library(epiUtils)

# Example cancer incidence data
cancer_data <- data.frame(
  age_group = c("0-19", "20-39", "40-59", "60-79", "80+"),
  events = c(2L, 8L, 45L, 120L, 89L),
  person_years = c(50000, 65000, 55000, 40000, 15000),
  standard_pop = c(35000, 30000, 25000, 20000, 10000)
)

# Calculate age-standardised rates
result <- calculate_asr_direct(cancer_data)
#> Warning: ! Found age groups with small/zero case counts:
#> • < 5 cases: 0-19 (2 cases)
#> ℹ Small and zero case counts may produce unstable rate estimates
#> ℹ Consider wider age groups, longer follow-up, or data quality checks

# View results
cat("Age-Standardised Rate:", round(result$asr_scaled, 1), "per 100,000\n")
#> Age-Standardised Rate: 120.7 per 100,000
cat("95% CI:", round(result$ci_lower_scaled, 1), "-", round(result$ci_upper_scaled, 1), "\n")
#> 95% CI: 106.4 - 136.5
```

### Person-Years Calculation

Calculate person-years of follow-up by age strata for cohort studies:

``` r
# Example cohort data
cohort_data <- data.frame(
  patient_id = 1:3,
  dob = as.Date(c("1960-01-15", "1955-06-20", "1972-09-01")),
  study_entry_date = as.Date(c("2020-03-01", "2021-01-01", "2019-01-01")),
  study_exit_date = as.Date(c("2024-06-15", "2025-12-31", "2022-04-10")),
  event_status = c(1L, 0L, 1L)
)

# Calculate person-years by age groups
py_result <- calculate_person_years_by_age_strata(cohort_data)
print(py_result)
#> # A tibble: 20 × 4
#>    age_group person_years events     n
#>    <fct>            <dbl>  <int> <int>
#>  1 [0-5)            0          0     0
#>  2 [5-10)           0          0     0
#>  3 [10-15)          0          0     0
#>  4 [15-20)          0          0     0
#>  5 [20-25)          0          0     0
#>  6 [25-30)          0          0     0
#>  7 [30-35)          0          0     0
#>  8 [35-40)          0          0     0
#>  9 [40-45)          0          0     0
#> 10 [45-50)          3.27       1     1
#> 11 [50-55)          0          0     0
#> 12 [55-60)          0          0     0
#> 13 [60-65)          4.29       1     1
#> 14 [65-70)          4.46       0     1
#> 15 [70-75)          0.533      0     1
#> 16 [75-80)          0          0     0
#> 17 [80-85)          0          0     0
#> 18 [85-90)          0          0     0
#> 19 [90-95)          0          0     0
#> 20 [95-100)         0          0     0
```

### WHO Standard Population

Access the built-in [WHO 2000-2025 World Standard
Population](https://seer.cancer.gov/stdpopulations/world.who.html):

``` r
# Load WHO standard population
data("who_2000_2025_standard_population")

# View structure
head(who_2000_2025_standard_population)
#> # A tibble: 6 × 6
#>   age_group who_world_standard_perc…¹ recalculation_to_add…² rounded_to_integers
#>   <chr>                         <dbl>                  <dbl>               <int>
#> 1 0-4                            8.86                 88569.               88569
#> 2 5-9                            8.69                 86870.               86870
#> 3 10-14                          8.6                  85970.               85970
#> 4 15-19                          8.47                 84670.               84670
#> 5 20-24                          8.22                 82171.               82171
#> 6 25-29                          7.93                 79272.               79272
#> # ℹ abbreviated names: ¹​who_world_standard_percent,
#> #   ²​recalculation_to_add_to_1million
#> # ℹ 2 more variables: standard_for_seer_stat <int>, lower_age_limit <int>
```

## Advanced Features

### Smart Case Count Handling

Warnings are raised for small case counts:

``` r
# Data with small case counts
small_counts_data <- data.frame(
  age_group = c("0-20", "20-40", "40-60"),
  events = c(0L, 3L, 15L),  # Zero and small case counts
  person_years = c(10000, 15000, 20000),
  standard_pop = c(15000, 20000, 25000)
)

# Calculate with warnings (default)
result_with_warnings <- calculate_asr_direct(small_counts_data)
#> Warning: ! Found age groups with small/zero case counts:
#> • zero cases: 0-20 and < 5 cases: 20-40 (3 cases)
#> ℹ Small and zero case counts may produce unstable rate estimates
#> ℹ Consider wider age groups, longer follow-up, or data quality checks

# Calculate silently when appropriate
result_silent <- calculate_asr_direct(small_counts_data, warn_small_cases = FALSE)
```

### Confidence Interval Methods

Choose between two confidence interval calculation methods:

``` r
# Gamma method (default) - more conservative for small counts and low rates
result_gamma <- calculate_asr_direct(small_counts_data, warn_small_cases = FALSE)

# Byar's/Dobson method - traditional epidemiological approach  
result_byars <- calculate_asr_direct(small_counts_data, ci_method = "byars", warn_small_cases = FALSE)

cat("Gamma CI:", round(result_gamma$ci_lower_scaled, 1), "-", round(result_gamma$ci_upper_scaled, 1), "\n")
#> Gamma CI: 22.5 - 60.5
cat("Byar's CI:", round(result_byars$ci_lower_scaled, 1), "-", round(result_byars$ci_upper_scaled, 1), "\n")
#> Byar's CI: 19.9 - 55.9
```

### Integration with Standard Populations

Easily combine your data with WHO standard populations:

``` r
# Using WHO standard population directly
who_subset <- who_2000_2025_standard_population[1:10, ]

# Create matching incidence data
incidence_data <- who_subset |>
  dplyr::mutate(
    events = as.integer(c(5, 8, 12, 18, 25, 35, 45, 60, 75, 85)),
    person_years = c(50000, 55000, 60000, 58000, 52000, 
                     48000, 42000, 35000, 25000, 15000),
    standard_pop = standard_for_seer_stat
  ) |>
  dplyr::select(age_group, events, person_years, standard_pop)

# Calculate ASR
who_result <- calculate_asr_direct(incidence_data)
cat("ASR using WHO standard:", round(who_result$asr_scaled, 1), "per 100,000\n")
#> ASR using WHO standard: 116 per 100,000
```

## Citation

When using `epiUtils` in publications, please cite:

``` r
citation("epiUtils")
```

## Contributing

Contributions are welcome! Please see the contributing guidelines and
submit issues or pull requests on GitHub.
