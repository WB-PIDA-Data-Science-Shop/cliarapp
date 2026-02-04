#' Identify unique IDs with available data
#'
#' Returns the set of unique identifiers (\code{id}) for which the corresponding
#' \code{value} is non-missing.
#'
#' @param value A vector (typically numeric or logical) indicating data availability
#'   (non-\code{NA} entries are considered available).
#' @param id A vector of identifiers of the same length as \code{value}.
#'
#' @return A vector of unique IDs with non-missing values.
#' @keywords coverage availability
#' @export
calculate_global_coverage <- function(value, id) {
  if (length(value) != length(id)) {
    stop("`value` and `id` must have the same length.")
  }
  mask <- !is.na(value)
  unique(id[mask])
}

#' Compute the available year range
#'
#' Returns the minimum and maximum available years as a string of the form
#' \code{"yyyy-yyyy"}. If no available years are found, returns \code{"Not available"}.
#'
#' @param value A vector (typically numeric or logical) indicating data availability.
#' @param time_id A vector of time identifiers (e.g., years) of the same length as \code{value}.
#'
#' @return A character string of the form \code{"yyyy-yyyy"} or \code{"Not available"}.
#' @keywords coverage time-range
#' @export
coverage_range_global <- function(value, time_id) {
  if (length(value) != length(time_id)) {
    stop("`value` and `time_id` must have the same length.")
  }
  mask <- !is.na(value) & !is.na(time_id)
  if (!any(mask)) return("Not available")
  
  yrs <- sort(unique(time_id[mask]))
  paste0(min(yrs, na.rm = TRUE), "-", max(yrs, na.rm = TRUE))
}

#' List available years
#'
#' Returns a comma-separated string of available years for which \code{value} is non-missing.
#' If no years are available, returns \code{"Not available"}.
#'
#' @param value A vector (typically numeric or logical) indicating data availability.
#' @param time_id A vector of time identifiers (e.g., years) of the same length as \code{value}.
#'
#' @return A character string with comma-separated years or \code{"Not available"}.
#' @keywords coverage years availability
#' @export
coverage_years_global <- function(value, time_id) {
  if (length(value) != length(time_id)) {
    stop("`value` and `time_id` must have the same length.")
  }
  mask <- !is.na(value) & !is.na(time_id)
  if (!any(mask)) return("Not available")
  
  yrs <- sort(unique(time_id[mask]))
  paste(yrs, collapse = ", ")
}

#' Calculate the share of available years
#'
#' Computes the percentage of unique years with non-missing values over the total
#' number of unique years in \code{time_id}. Returns \code{NA_real_} if no years are defined.
#'
#' @param value A vector (typically numeric or logical) indicating data availability.
#' @param time_id A vector of time identifiers (e.g., years) of the same length as \code{value}.
#'
#' @return A numeric scalar representing the percentage of available years (0–100),
#'   or \code{NA_real_} if no years are defined.
#' @keywords coverage share percent
#' @export
coverage_share_global <- function(value, time_id) {
  if (length(value) != length(time_id)) {
    stop("`value` and `time_id` must have the same length.")
  }
  all_years <- sort(unique(time_id[!is.na(time_id)]))
  if (length(all_years) == 0) return(NA_real_)
  
  avail_years <- sort(unique(time_id[!is.na(value) & !is.na(time_id)]))
  if (length(avail_years) == 0) return(NA_real_)
  
  round(100 * length(avail_years) / length(all_years), 2)
}

#' Compute coverage metrics by country and indicator
#'
#' Aggregates coverage statistics by \code{country_id} and \code{indicator_id}, including:
#' the range of available years, the list of available years, and the share of years
#' with data.
#'
#' @param data A data frame containing the required columns.
#' @param country_id Unquoted column name identifying countries (tidy evaluation).
#' @param indicator_id Unquoted column name identifying indicators (tidy evaluation).
#' @param time_id Unquoted column name with time identifiers (e.g., years).
#' @param value_column Unquoted column name containing the values whose availability
#'   is being assessed.
#'
#' @return A tibble with one row per \code{country_id} × \code{indicator_id} and the following columns:
#' \itemize{
#'   \item \code{year_range}: character string with the available year range.
#'   \item \code{available_years}: character string with comma-separated available years.
#'   \item \code{available_share}: numeric percentage of available years.
#' }
#'
#' @importFrom dplyr group_by summarise arrange across everything
#' @importFrom data.table as.data.table
#' @keywords coverage summary aggregation
#' @export
compute_global_coverage <- function(data, country_id, indicator_id, time_id, value_column) {
  data |>
    data.table::as.data.table() |>
    dplyr::group_by({{ country_id }}, {{ indicator_id }}) |>
    dplyr::summarise(
      year_range      = coverage_range_global({{ value_column }}, {{ time_id }}),
      available_years = coverage_years_global({{ value_column }}, {{ time_id }}),
      available_share = coverage_share_global({{ value_column }}, {{ time_id }}),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::everything()))
}
