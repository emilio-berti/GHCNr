#' @title Number of Days in Month
#' @importFrom tibble tibble
#' @export
#' @return Table with number of days in the months.
.days_in_month <- function() {
  ans <- tibble(
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") ,
    days = c(  31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  )
  return(ans)
}

#' @title Check Ahich Variables Are Absent 
#' @importFrom tibble tibble
#' @export
#' @param x Object of class `ghcn_daily`.
#' @return Character vector
.missing_variables <- function(x) {
  stopifnot(inherits(x, "ghcn_daily") || inherits(x, "ghcn_monthly") || inherits(x, "ghcn_annual"))
  ans <- setdiff(c("tavg", "tmin", "tmax", "prcp"), colnames(x))
  return(ans)
}

#' @title Add Columns to Handle Summarize
#' @importFrom tibble tibble
#' @export
#' @param x Object of class `ghcn_daily`.
#' @return Table with number of days in the months.
.add_variables <- function(x) {
  stopifnot(inherits(x, "ghcn_daily") || inherits(x, "ghcn_monthly") || inherits(x, "ghcn_annual"))

  # add variables to not break summarize
  if (!all(c("tavg", "tmin", "tmax", "prcp") %in% colnames(x))) {
    missing_variables <- .missing_variables(x)
    for (v in missing_variables) {
      x[[v]] <- -9999
    }
  }
  return(x)
}

#' @title Calculate Coverage of Daily Summaries
#'
#' @importFrom dplyr mutate group_by tally add_tally ungroup select across distinct_all
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na
#' @importFrom tidyselect where matches any_of
#' @importFrom rlang .data
#' @importFrom tibble tibble as_tibble
#'
#' @export
#'
#' @param x Object of class `ghcn_daily`.
#'
#' @details This function calculates the temporal coverage of stations.
#' It returns a table with:
#' \itemize{
#'  \item mothly_coverage The proportion of the days with records in the month
#'  \item annual_coverage The proportion of the days with records in the year
#'  \item annual_coverage The proportion of the years with records in the reference period
#' }
#' Important: that 'annual_coverage = 1' does not mean that all years have 'annual_coverage = 1', 
#' but rather that all years have at least one record.
#'
#' @return A tibble with the stations within the `roi`.
#' @examples
#' cleaned <- remove_flagged(CA003076680)
#' cover <- coverage(cleaned)
#' cover[cover$month == 1, ]
coverage <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))

  coverage <- x |> 
    drop_na() |> 
    select(-any_of(c("tmin", "tmax", "prcp"))) |> 
    mutate(
      year = format(.data$date, "%Y"),
      month = format(.data$date, "%m"),
      day = format(.data$date, "%d"),
    ) |>
    group_by(.data$station, .data$year, .data$month) |> 
    add_tally() |> 
    left_join(.days_in_month(), by = c("month")) |> 
    mutate(monthly_coverage = .data$n / .data$days) |> 
    ungroup() |> 
    select(-"n") |> 
    group_by(.data$station, .data$year) |> 
    add_tally() |> 
    mutate(
      annual_coverage = .data$n / 365,
      annual_coverage = ifelse(.data$annual_coverage > 1, 1, .data$annual_coverage)  # leap years
    ) |> 
    ungroup() |> 
    select(-"n", -"day", -"days", -"date") |> 
    distinct_all() |> 
    mutate(
      year = as.numeric(.data$year),
      month = as.numeric(.data$month)
    ) |> 
    mutate(
      n = max(.data$year) - min(.data$year) + 1,
      year_coverage = length(unique(.data$year)) / .data$n
    ) |> 
    select(-"n")

  return(coverage)
}

#' @title Calculate Minimum
#' @export
#' @param x Numeric vector
#' @return Numeric.
.min <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, min(x), NA)
    return(ans)
}
#' @title Calculate Maximum
#' @export
#' @param x Numeric vector
#' @return Numeric.
.max <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, max(x), NA)
    return(ans)
}
#' @title Calculate Mean
#' @export
#' @param x Numeric vector
#' @return Numeric.
.mean <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, mean(x), NA)
    return(ans)
}
#' @title Calculate Sum
#' @export
#' @param x Numeric vector
#' @return Numeric.
.sum <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, sum(x), NA)
    return(ans)
}