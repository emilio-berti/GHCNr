#' @title Calculate Quarterly Timseries
#'
#' @description
#' `quarterly()` aggregates the daily timeseries into a quarterly one.
#' Aggregation is done differently for TMIN, TMAX, and PRCP.
#'
#' @details
#' Quarters are defined as:
#' \describe{
#'  \item{first}{January to March}
#'  \item{second}{April to June}
#'  \item{third}{July to September}
#'  \item{fourth}{October to December}
#' }
#' Aggregation is done as:
#' \describe{
#'  \item{TMAX}{Maximum temperature recorded in the quarter} 
#'  \item{TMIN}{Minimum temperature recorded in the quarter}
#'  \item{PRCP}{Total (cumulative) precipitation amount in the quarter}
#' }
#'
#' @importFrom dplyr select mutate summarize group_by case_when
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @return A tibble with the quarterly timeseries at the stations.
#'
#' @examples
#' quarterly(CA003076680)
quarterly <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))

  .check_flags(x)
  x <- .drop_flags(x)
  missing_variable <- .missing_variables(x)
  x <- .add_variables(x)

  ans <- x |>
    mutate(
      year = format(.data$date, "%Y"),
      month = as.numeric(format(.data$date, "%m")),
      quarter = case_when(
        month < 4 ~ 1,
        month >= 4 & month < 7 ~ 2,
        month >= 7 & month < 9 ~ 3,
        month >= 9 ~ 4,
      )
    ) |> 
    select(-"month") |> 
    group_by(.data$station, .data$year, .data$quarter) |>
    summarize(
      tmin = .min(.data$tmin),
      tmax = .max(.data$tmax),
      prcp = .sum(.data$prcp),
      .groups = "drop"
    ) |>
    select(-all_of(missing_variable)) |> 
    mutate(year = as.numeric(.data$year))
  
  ans <- .s3_quarterly(ans)
  
  return(ans)
}
