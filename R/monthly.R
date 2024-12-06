#' @title Download Monthly Summaries
#'
#' @importFrom dplyr filter group_by tally select left_join summarize
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @export
#'
#' @param station_id Character, station id(s).
#' @param start_date Character, start date.
#' @param end_date Character, end date.
#' @param variables Character, vector of the variables to include.
#' @param remove_flagged Logical, if to remove flagged records.
#' @param only_complete Logical, if to retain only months with complete records.
#'
#' @details
#' \emph{station_id} can be a vector with multiple stations.
#' Dates should be given in `YYYY-mm-dd` format.
#' Monthly summaries are derived from the daily timeseries.
#' February is always assumed to have 28 days when calculating coverage;0
#' in leap years, values of coverage > 1 are set to 1.
#'
#' @return A tibble with the monthly timeseries at the stations.
monthly <- function(
  station_id,
  start_date,
  end_date,
  variables = c("tmax", "tmin", "prcp"),
  remove_flagged = FALSE,
  only_complete = FALSE
) {

  days_in_month <- tibble(
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") ,
    days = c(  31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  )

  d <- daily(
    station_id,
    start_date,
    end_date,
    variables,
    remove_flagged = TRUE
  ) |>
    mutate(
      year = format(date, "%Y"),
      month = format(date, "%m")
    )

  coverage <- d |>
    group_by(.data$year, .data$month) |>
    tally() |>
    ungroup() |>
    left_join(days_in_month, by = "month") |>
    mutate(monthly_coverage = .data$n / .data$days)

  d <- d |>
    left_join(coverage, by = c("year", "month")) |>
    mutate(
      monthly_coverage = ifelse(  # for leap years
        .data$monthly_coverage > 1,
        1,
        .data$monthly_coverage
      )
    )
  if (only_complete) {
    d <- d |> filter(.data$monthly_coverage == 1)
  }

  # add variables to not break summarize
  if (!all(c("tavg", "tmin", "tmax", "prcp") %in% colnames(d))) {
    to_add <- setdiff(c("tavg", "tmin", "tmax", "prcp"), colnames(d))
    for (x in to_add) {
      d[[x]] <- -9999
    }
  }

  # group also by coverage to keep it
  ans <- d |>
    group_by(.data$year, .data$month, .data$monthly_coverage) |>
    summarize(
      tmin = min(.data$tmin),
      tmax = max(.data$tmax),
      tavg = mean(.data$tavg),
      prcp = sum(.data$prcp),
      .groups = "drop"
    ) |>
    select(-all_of(to_add))

  return(ans)
}
