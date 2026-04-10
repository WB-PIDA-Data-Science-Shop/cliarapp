
# Function: def_quantiles
#
# Computes percentile-based benchmarking for a base country against a set of
# comparison countries on a cross-sectional (static) snapshot of indicators.
#
# The output feeds the **Static Benchmark Plot** (`output$plot`) and the
# **PowerPoint report export** in the CLIAR dashboard. Each indicator row in the
# resulting data frame carries the columns needed to draw the three-colour
# segment chart (Weak / Emerging / Strong).
#
# Workflow:
#   1. Identifies indicators that are entirely NA for the base country and
#      excludes them so the plot never shows empty rows.
#   2. Filters the data to the base country + comparison countries.
#   3. Pivots to long format and joins the variable dictionary for display names.
#   4. Within each indicator, computes:
#        - `dtt`        – percent-rank (0–1) of the raw value (closeness to frontier)
#        - `q_lv_25`    – 25th-percentile value (used to detect low variance)
#        - `q_lv_75`    – 75th-percentile value (used to detect low variance)
#        - `q_cutoff1`  – lower cutoff quantile (25th or 33rd percentile)
#        - `q_cutoff2`  – upper cutoff quantile (50th or 66th percentile)
#        - `status`     – categorical label: "Weak", "Emerging", or "Strong"
#        - `nrank`      – rank among comparison countries (1 = highest value)
#   5. Renames the raw `value` column to `dtf` (distance to frontier).
#   6. Drops indicators with zero variance (q_lv_25 == q_lv_75) for the base
#      country, except family-average variables (*_avg).
#
# @param data        Data frame with country-level indicators in wide format.
#                    Must contain `country_name` and indicator columns starting
#                    from column 6 onward.
# @param base_country    Character. Name of the base (focus) country.
# @param country_list    Data frame with a `country_name` column listing all
#                        available countries.
# @param comparison_countries Character vector of country names to benchmark against.
# @param vars             Character vector of indicator column names to include.
# @param variable_names   Data frame (variable dictionary) with at least columns
#                         `variable` and `var_name`, used to map internal column
#                         names to display labels.
# @param threshold        Character. Either "Default" (cutoffs at 25/50) or
#                         "Terciles" (cutoffs at 33/66).
#
# @return A tibble with one row per country × indicator containing the columns:
#         `variable`, `dtf`, `var_name`, `dtt`, `q_lv_25`, `q_lv_75`,
#         `q_cutoff1`, `q_cutoff2`, `status`, `nrank`, plus any extra columns
#         inherited from `variable_names`.

def_quantiles <- function(data, base_country, country_list, comparison_countries, vars, variable_names, threshold) {

# --- Step 1: Identify comparison countries ------------------------------------
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

# --- Step 2: Drop indicators that are fully NA for the base country -----------
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names

# --- Step 3: Build final variable list (requested minus missing) --------------
if(length(na_indicators) > 0){
  variables <-
    setdiff(vars, na_indicators)
  
  variables <-
    intersect(variables, names(data))
}else{
  variables <- vars
}


# --- Step 4: Filter to relevant countries and variables -----------------------
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      any_of(variables)
    )

# --- Step 5: Pivot to long format and join variable dictionary ----------------
  quantiles <-
    quantiles %>%

    # Make long: one row per country × indicator
    pivot_longer(
      cols = any_of(variables),
      names_to = "variable"
    ) %>%

    # Attach display names and family grouping from the variable dictionary
    left_join(
      variable_names,
      by = "variable"
    )

# --- Step 6: Determine cutoff thresholds from user selection ------------------
if (threshold=="Default"){
    cutoff<-c(25,50)
}else if (threshold=="Terciles")
{
  cutoff<-c(33,66)
}

# --- Step 7: Compute benchmarking metrics per indicator -----------------------
# For each indicator (grouped by variable + var_name):
#   - dtt:       percent_rank of the raw value (0 = lowest, 1 = highest)
#   - q_lv_25/75: 25th and 75th percentile of the raw value distribution
#   - q_cutoff1/2: quantile thresholds that define the colour segments
#   - status:    categorical label derived from dtt vs cutoffs
#   - nrank:     dense rank where 1 = highest raw value
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q_lv_25 = quantile(value,c(0.25)),
      q_lv_75 = quantile(value,c(0.75)),
      q_cutoff1 = quantile(value, c(cutoff[1]/100)),
      q_cutoff2 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
      ),
      nrank = min_rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value)                        # rename raw value → dtf

  # --- Step 8: Remove low-variance indicators --------------------------------
  # If q_lv_25 == q_lv_75 for the base country, the indicator has no spread
  # among the comparison set and the segment chart would be meaningless.
  # Family-average variables (*_avg) are kept regardless.
  low_variance_indicators <-
    quantiles %>%
    filter(country_name == base_country & q_lv_25==q_lv_75) %>%
    select(variable) %>%
    unlist
  
  low_variance_indicators <- low_variance_indicators[!grepl("_avg", low_variance_indicators)]
  

  quantiles <-
    quantiles %>%
    filter(!(variable %in% low_variance_indicators))

}

# Function: def_quantiles_dyn
#
# Dynamic (panel / time-series) counterpart of `def_quantiles`. Computes the
# same benchmarking metrics but does so **per year**, so the dashboard can show
# how a country's relative position changes over time.
#
# The output feeds the **Dynamic Benchmark Plot** (`output$dynamic_benchmark_plot`)
# and the dynamic panel of the **PowerPoint report export**.
#
# Key differences from `def_quantiles`:
#   - The data includes a `year` column; grouping is by (variable, var_name, year).
#   - Missing-data detection uses the share of NAs across all years for the base
#     country (variables with 100 % NA are dropped), rather than a single-row check.
#   - Low-variance filtering is done inline (mutate + filter) instead of a
#     separate post-processing step.
#
# @param data        Data frame in wide format with `country_name`, `year`, and
#                    indicator columns.
# @param base_country    Character. Name of the base (focus) country.
# @param country_list    Data frame with a `country_name` column.
# @param comparison_countries Character vector of country names to benchmark against.
# @param vars             Character vector of indicator column names to include.
# @param variable_names   Variable dictionary (must contain `variable` column).
# @param threshold        "Default" (25/50) or "Terciles" (33/66).
#
# @return A tibble with one row per country × indicator × year containing:
#         `variable`, `dtf`, `var_name`, `dtt`, `q_lv_25`, `q_lv_75`,
#         `q_cutoff1`, `q_cutoff2`, `status`, `nrank`, `year`, plus columns
#         inherited from `variable_names`.


def_quantiles_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names,threshold) {
  # --- Step 1: Identify comparison countries ----------------------------------
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)
  
  
  # --- Step 2: Drop indicators 100 % NA for the base country ------------------
  # Unlike def_quantiles (single-row check), this looks across all years.
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) 
  
  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])
  
  na_indicators <- na_indicators[!grepl("_avg", na_indicators)]
  
  
  # --- Step 3: Build final variable list (requested minus missing) ------------
  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }

  # --- Step 4: Filter to relevant countries and variables ---------------------
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      year,
      any_of(variables)
    )
  
  quant_vars <- names(quantiles)[names(quantiles) %in% variables]
  
  # --- Step 5: Pivot to long format and join variable dictionary ---------------
  quantiles <-
    quantiles %>%
    
    # Make long: one row per country × indicator × year
    pivot_longer(
      cols = any_of(quant_vars),
      names_to = "variable"
    ) %>%
    
    # Attach display names and family grouping
    left_join(
      variable_names,
      by = "variable"
    )
  
  # --- Step 6: Determine cutoff thresholds from user selection ----------------
  if (threshold=="Default"){
    cutoff<-c(25,50)
  }else if (threshold=="Terciles")
  {
    cutoff<-c(33,66)
  }
  
  # --- Step 7: Compute benchmarking metrics per indicator per year ------------
  # Same metrics as def_quantiles, but grouped by (variable, var_name, year).
  # Low-variance indicators are filtered inline via a todrop flag.
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name, year) %>%
    mutate(
      dtt = percent_rank(value),
      q_lv_25 = quantile(value, c(0.25)),
      q_lv_75 = quantile(value, c(0.75)),
      q_cutoff1 = quantile(value, c(cutoff[1]/100)),
      q_cutoff2 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
      ),
      nrank = min_rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value) %>% 
    # Remove indicators where there is too little variance
    mutate(todrop = ifelse(country_name == base_country & q_lv_25==q_lv_75, 1, 0)) %>% 
    filter(todrop != 1) %>% 
    select(-todrop)
  

}
