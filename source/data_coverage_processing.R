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

ctf_coverage_country_complete <- compiled_indicators |> 
  # 2025 release, should take the static 2020 to 2024 period
  filter(between(year,2020,2024)) |> 
  pivot_longer(
    cols = -c(country_code, income_group, region, country_name, year),  # Exclude these columns
    names_to = "indicators", 
    values_to = "indicator_value"  
  ) %>% 
  left_join( # Join with db_variables
    db_variables |> select(variable, var_name, source, family_name, benchmarked_ctf),
    by = c("indicators" = "variable")
  )

# Save the processed data -------------------------------------------------

write_csv(
  ctf_coverage_country_complete,
  here(
    "data",
    "coverage_ctf_for_analysis.csv"
  )
)



