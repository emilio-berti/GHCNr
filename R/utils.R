#' @title Plot GHCN Timeseries
#' @param x Object of class ghcn_daily.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to \code{plot()}.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @examples
#' plot(CA003076680, "tmax")
plot.ghcn_daily <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_daily"))
  stopifnot(variable %in% c("tmin", "tmax", "prcp"))
  op <- par(no.readonly = TRUE)
  par(mar = c(8, 8, 1, 1))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Roma")
  }
  names(palette) <- unique(x[["station"]])
  plot(
    x$date, x[[variable]],
    xlab = "",
    ylab = toupper(variable),
    col = palette[x[["station"]]],
    pch = 3,
    type = "n",
    frame = FALSE,
    axes = FALSE,
    ...
  )
  axis.Date(
    1,
    at = seq(min(x$date), max(x$date) + 180, by = 90),
    format = "%Y-%m-%d",
    las = 2
  )
  yrange <- range(x[[variable]], na.rm = TRUE)
  axis(
    2,
    at = round(seq(floor(yrange[1]), ceiling(yrange[2]), length.out = 5))
  )
  for (s in unique(x[["station"]])) {
    lines(
      x$date[x$station == s], x[[variable]][x$station == s],
      col = palette[s]
    )
  }
  par(op)
}

#' @title Plot GHCN Timeseries
#' @param x Object of class ghcn_monthly.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to \code{plot()}.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @examples
#' plot(monthly(CA003076680), "tmax")
plot.ghcn_monthly <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_monthly"))
  stopifnot(variable %in% c("tmin", "tmax", "prcp"))
  op <- par(no.readonly = TRUE)
  par(mar = c(8, 8, 1, 1))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Roma")
  }
  names(palette) <- unique(x[["station"]])
  years <- range(x[["year"]])
  x$date <- as.Date(paste(x$year, x$month, "01", sep = "-"))
  plot(
    x$date, x[[variable]],
    xlab = "",
    ylab = toupper(variable),
    col = palette[x[["station"]]],
    pch = 3,
    type = "n",
    frame = FALSE,
    axes = FALSE,
    ...
  )
  axis.Date(
    1,
    at = seq(min(x$date), max(x$date) + 180, by = 90),
    format = "%Y-%m-%d",
    las = 2
  )
  yrange <- range(x[[variable]], na.rm = TRUE)
  axis(
    2,
    at = round(seq(floor(yrange[1]), ceiling(yrange[2]), length.out = 5))
  )
  for (s in unique(x[["station"]])) {
    lines(
      x$date[x$station == s], x[[variable]][x$station == s],
      col = palette[s],
      lw = 2
    )
  }
  par(op)
}

#' @title Plot GHCN Timeseries
#' @param x Object of class ghcn_annual.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to \code{plot()}.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @examples
#' plot(annual(CA003076680), "tmax")
plot.ghcn_annual <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_annual"))
  stopifnot(variable %in% c("tmin", "tmax", "prcp"))
  op <- par(no.readonly = TRUE)
  par(mar = c(8, 8, 1, 1))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Roma")
  }
  names(palette) <- unique(x[["station"]])
  years <- range(x[["year"]])
  plot(
    x$year, x[[variable]],
    xlab = "",
    ylab = toupper(variable),
    col = palette[x[["station"]]],
    pch = 3,
    type = "n",
    frame = FALSE,
    axes = FALSE,
    ...
  )
  axis(1, at = seq(years[1], years[2], by = 1), las = 2)
  yrange <- range(x[[variable]], na.rm = TRUE)
  axis(
    2,
    at = round(seq(floor(yrange[1]), ceiling(yrange[2]), length.out = 5))
  )
  for (s in unique(x[["station"]])) {
    lines(
      x$year[x$station == s], x[[variable]][x$station == s],
      col = palette[s],
      lw = 3
    )
  }
  par(op)
}

#' @title GHCNd Flags
#' @importFrom tibble tibble
#' @export
#' @param strict Logical, if to include all flags (TRUE) or not (FALSE).
#' @details <https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>
#' @return Table with flags.
.flags <- function(strict) {
  ans <- tibble(
    "D" = "duplicate flag",
    "I" = "consistency flag",
    "K" = "streak flag",
    "M" = "mega flag",
    "N" = "naught flag",
    "R" = "lagged range flag",
    "X" = "bounds flag"
  )
  if (strict) {
  ans <- ans |>
    mutate(
      "O" = "outlier flag",
      "G" = "gap flag", 
      "L" = "multiday flag",
      "S" = "spatial consistency flag",
      "Z" = "Datzilla flag"
    )
  }
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