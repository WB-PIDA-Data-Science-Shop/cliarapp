
compute_global_coverage <- function(data, country_id, indicator_id, time_id, value_column) {
  global_data_coverage <- data |>
    data.table::as.data.table() |>
    group_by({{country_id}}, {{indicator_id}}) |>
    summarise(
      year_range = coverage_range_global({{value_column}}, {{time_id}}),
      available_years = coverage_years_global({{value_column}}, {{time_id}}),
      available_share = coverage_share_global({{value_column}}, {{time_id}}),
      .groups = 'drop'
    )
  
  return(global_data_coverage)
}

scale_values <- function(x){
  (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}
