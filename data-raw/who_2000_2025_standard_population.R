## code to prepare `who_2000_2025_standard_population` dataset goes here

library(rvest)

who_2000_2025_standard_population <- read_html("https://seer.cancer.gov/stdpopulations/world.who.html") |>
  html_element(".even_alternating") |>
  html_table() |>
  rename(age_group = `Age Group`,
         who_world_standard_percent = `WHO World Standard (%)`,
         recalculation_to_add_to_1million = `Recalculation to add to 1,000,000`,
         rounded_to_integers = `Rounded to Integers`,
         standard_for_seer_stat = `Standard For SEER*Stat`) |>
  mutate(across(all_of(c("rounded_to_integers", "standard_for_seer_stat")),
                \(x) x |>
                  stringr::str_remove_all(",") |>
                  stringr::str_remove_all("\\*") |>
                  as.integer())) |>
  filter(age_group != "Total") |>
  mutate(lower_age_limit = as.integer(stringr::str_remove(age_group, "[-|+].*")),
         upper_age_limit = as.integer(stringr::str_remove(age_group, ".*[-|+]"))) |>
  mutate(age_group = case_when(age_group == "100+" ~ "[100+)",
                               TRUE ~ paste0("[", lower_age_limit, "-", upper_age_limit + 1, ")"))) |>
  select(-upper_age_limit)

# ensure no NA values inadvertently introduced
stopifnot(identical(who_2000_2025_standard_population, na.omit(who_2000_2025_standard_population)))

usethis::use_data(who_2000_2025_standard_population, overwrite = TRUE)
