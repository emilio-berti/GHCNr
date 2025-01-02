#' @title Temperature Anomaly
#'
#' @description
#' `anomaly()` calculates the temperature anomalies compared to a baseline
#' reference period. Anomalies are the difference between annual temperature
#' extremes and the average across the baseline period.
#'
#' @details
#' `cutoff` must be a character with the date, e.g. "2000-01-01".
#'
#' @importFrom dplyr filter select mutate summarize group_by left_join
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @export
#'
#' @param x Object of class `ghcn_daily` or `ghcn_annual`. See [daily()] and [annual()] for details.
#' @param cutoff Numeric, last year of the baseline period (inclusive).
#' @return A tibble with the anomaly timeseries at the stations.
#'
#' @examples
#' x <- USC00010655
#' x <- remove_flagged(x)
#' cover <- annual_coverage(x)
#' years <- cover$year[cover$"annual_coverage_tmax" > .99 & cover$"annual_coverage_tmin" > .99]
#' years <- setdiff(years, 2024)
#' x$years <- as.numeric(format(x$date, "%Y"))
#' x <- x[x$years %in% years, ]
#' a <- annual(x)
#' anom <- anomaly(a, cutoff = 2012)
#' plot(anom)
anomaly <- function(x, cutoff) {
  stopifnot(inherits(x, c("ghcn_annual", "ghcn_daily")))
  if (inherits(x, "ghcn_daily")) {
    .check_flags(x)
    x <- .drop_flags(x)
    x <- annual(x)
  }
  stopifnot(inherits(cutoff, "numeric"))

  missing_variable <- .missing_variables(x)
  x <- .add_variables(x)

  baseline <- x |> 
    filter(.data$year <= cutoff) |> 
    group_by(.data$station) |> 
    summarize(
      baseline_tmin = mean(.data$tmin, na.rm = TRUE),
      baseline_tmax = mean(.data$tmax, na.rm = TRUE),
      .groups = "drop"
    )

  ans <- x |> 
    left_join(baseline, by = "station") |> 
    mutate(
      tmin = .data$tmin - .data$baseline_tmin,
      tmax = .data$tmax - .data$baseline_tmax
    )

  ans <- ans |> 
    group_by(.data$year) |> 
    summarize(
      tmin = mean(.data$tmin, na.rm = TRUE),
      tmax = mean(.data$tmax, na.rm = TRUE)
    ) |> 
    .s3_anomaly()

  return(ans)

}
