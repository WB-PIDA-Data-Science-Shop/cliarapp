###############################################
# This script processes the compiled indicators data to analyze data coverage
# across different dimensions such as country, year, and indicator.
###############################################


# load-libraries ----------------------------------------------------------

library(readr)
library(dplyr)
library(here)
library(tidyr)
library(stringr) 

source(
  here("source","funs_coverage.R")
)

# read-in -----------------------------------------------------------------

# Read in the compiled indicators data
compiled_indicators <- readRDS(
  here("data", "compiled_indicators.rds")
)

# Read in the metadata
# install.packages("pak")
# install.packages("remotes")
# install.packages('pointblank')
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")

library(cliaretl)
db_variables <- cliaretl::db_variables

# Extract the reference year
ref_year <- attr(db_variables, "ref_year")
print(ref_year) 

vars_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable)

# data_processing ---------------------------------------------------------

ctf_year_long_diagnosis <- compiled_indicators |> 
  filter(between(year,2019,2023)) |> 
  pivot_longer(
    cols = -c(country_code, income_group, region, country_name, year),  # Exclude these columns
    names_to = "indicators", 
    values_to = "indicator_value"  
  ) 

# Calculate coverage for CTF benchmarked indicators using the custom function
ctf_global_coverage <- ctf_year_long_diagnosis |>
  select(-income_group, -region) |>  
  compute_global_coverage(country_name, indicators, year, indicator_value)



# Renaming and joining to db_variables classification 
ctf_coverage_country_complete <- ctf_global_coverage |>
  mutate(
    available_share = case_when(
      !is.na(available_share) ~ as.numeric(str_remove(available_share, "%")),
      TRUE ~ NA_real_) 
  )|>
  left_join( # Join with db_variables
    db_variables |> select(variable, var_name, source, family_name, benchmarked_ctf),
    by = c("indicators" = "variable")
  ) |>
  left_join( # Join with country metadata
    compiled_indicators |> 
      select(country_name, country_code, income_group, region) |> 
      distinct(),
    by = "country_name"
  ) |> 
  select( # Select and reorder columns
    country_code,country_name, income_group, region,
    indicators, year_range, available_years, available_share,
    var_name, family_name, source, benchmarked_ctf
  )

# Save the processed data -------------------------------------------------

write_csv(
  ctf_coverage_country_complete,
  here(
    "data",
    "coverage_ctf_for_analysis.csv"
  )
)



