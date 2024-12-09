#' @title GHCNd Flags
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @details <https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>
#' @return Table with flags.
.flags <- function() {
  ans <- tibble(
    "D" = "duplicate flag",
    "I" = "consistency flag",
    "K" = "streak flag",
    "M" = "mega flag",
    "N" = "naught flag",
    "R" = "lagged range flag"
  )
  return(ans)
}

#' @title Extract GHCNd Flags
#'
#' @export
#'
#' @param x Character, vector of the flag as returned by the GHCNd API call.
#'
#' @details <https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>
#' @return Character of the flag.
.extract_flag <- function(x) {
  x <- sapply(x, \(x) ifelse(is.na(x), ",,,", x))
  x <- sapply(x, \(x) strsplit(x, ",")[[1]][2])
  return(x)
}

#' @title Number of Days in Month
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @return Table with number of days in the months.
.days_in_month <- function() {
  ans <- tibble(
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") ,
    days = c(  31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  )
  return(ans)
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
#' @param x Object of class `daily`.
#'
#' @details dates should be given in `YYYY-mm-dd` format.
#'
#' @return A tibble with the stations within the `roi`.
coverage <- function(x) {
  stopifnot(inherits(x, "ghcn-daily"))

  coverage <- x |> 
    drop_na() |> 
    select(-any_of(c("tmin", "tmax", "prcp"))) |> 
    mutate(
      year = format(.data$date, "%Y"),
      month = format(.data$date, "%m"),
      day = format(.data$date, "%d"),
    ) |>
    group_by(.data$year, .data$month) |> 
    add_tally() |> 
    left_join(.days_in_month(), by = "month") |> 
    mutate(monthly_coverage = .data$n / .data$days) |> 
    ungroup() |> 
    select(-"n") |> 
    group_by(.data$year) |> 
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