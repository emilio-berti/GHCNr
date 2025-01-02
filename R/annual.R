#' @title Calculate Annual Timseries
#'
#' @description
#' `annual()` aggregates the daily timeseries into an annual one.
#' Aggregation is done differently for TMIN, TMAX, and PRCP.
#'
#' @details
#' Aggregation is done as:
#' \describe{
#'  \item{TMAX}{Maximum temperature recorded in the year} 
#'  \item{TMIN}{Minimum temperature recorded in the year}
#'  \item{PRCP}{Total (cumulative) precipitation amount in the year}
#' }
#'
#' @importFrom dplyr select mutate across distinct_all summarize group_by
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @return A tibble with the annual timeseries at the stations.
#'
#' @examples
#' annual(CA003076680)
annual <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))

  .check_flags(x)
  x <- .drop_flags(x)
  missing_variable <- .missing_variables(x)
  x <- .add_variables(x)

  ans <- x |>
    mutate(year = format(.data$date, "%Y")) |> 
    group_by(.data$station, .data$year) |>
    summarize(
      tmin = .min(.data$tmin),
      tmax = .max(.data$tmax),
      prcp = .sum(.data$prcp),
      .groups = "drop"
    ) |>
    select(-all_of(missing_variable)) |> 
    mutate(year = as.numeric(.data$year))
  
  ans <- .s3_annual(ans)
  
  return(ans)
}
