#' @title Calculate Monthly Coverage
#'
#' @description
#' `monthly_coverage()` calculates how many days have been recorded for 
#' each month in the time period.
#'
#' @details
#' To calculate the coverage, a full daily time range is full joined to the
#' timeseries. Missing days are set to NA. Coverage is then calculated as
#' the number of values that are not NAs over the number of NAs.
#'
#' @importFrom dplyr mutate group_by full_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @return A table with mothly coverage.
#'
#' @examples
#' cleaned <- remove_flagged(CA003076680)
#' cover <- monthly_coverage(cleaned)
#' cover[cover$year == 2020, ]
monthly_coverage <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))
  .check_flags(x)
  x <- .drop_flags(x)

	timespan <- seq.Date(min(x$date), max(x$date), by = "day")

	x <- tibble(date = timespan) |> 
		full_join(x, by = "date", multiple = "all") |> 
    mutate(
    	year = as.numeric(format(date, "%Y")),
    	month = as.numeric(format(date, "%m"))
    ) |> 
    as_daily()

  missing_variables <- .missing_variables(x)
  if (length(missing_variables > 0)) x <- .add_variables(x)

  ans <- x |> 
    pivot_longer(
    	cols = c("tmax", "tmin", "prcp"),
    	names_to = "variable",
    	values_to = "value"
    ) |> 
    mutate(value = ifelse(.data$value == -9999, NA, .data$value)) |> 
    group_by(.data$year, .data$month, .data$variable) |> 
    summarize(
    	coverage = sum(!is.na(.data$value)) / length(.data$value),
    	.groups = "drop"
    ) |> 
    filter(!.data$variable %in% missing_variables) |> 
    pivot_wider(names_from = "variable", values_from = "coverage") |> 
    rename_with(
    	~ paste("monthly", "coverage", .x, sep = "_"),
    	any_of(c("tmax", "tmin", "prcp"))
  )
  return (ans)
}

#' @title Calculate Annual Coverage
#'
#' @description
#' `annual_coverage()` calculates how many days have been recorded for 
#' each year in the time period.
#'
#' @details
#' To calculate the coverage, a full daily time range is full joined to the
#' timeseries. Missing days are set to NA. Coverage is then calculated as
#' the number of values that are not NAs over the number of NAs.
#'
#' @importFrom dplyr mutate group_by full_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @return A table with annual coverage.
#'
#' @examples
#' cleaned <- remove_flagged(CA003076680)
#' cover <- annual_coverage(cleaned)
#' cover
annual_coverage <- function(x) {
	stopifnot(inherits(x, "ghcn_daily"))
  .check_flags(x)
  x <- .drop_flags(x)

  timespan <- seq.Date(min(x$date), max(x$date), by = "day")

	x <- tibble(date = timespan) |> 
		full_join(x, by = "date", multiple = "all") |> 
    mutate(
    	year = as.numeric(format(date, "%Y")),
    	month = as.numeric(format(date, "%m"))
    ) |> 
    as_daily()

  missing_variables <- .missing_variables(x)
  if (length(missing_variables > 0)) x <- .add_variables(x)

  ans <- x |> 
    pivot_longer(
    	cols = c("tmax", "tmin", "prcp"),
    	names_to = "variable",
    	values_to = "value"
    ) |> 
    mutate(value = ifelse(.data$value == -9999, NA, .data$value)) |> 
    group_by(.data$year, .data$variable) |> 
    summarize(
    	coverage = sum(!is.na(.data$value)) / length(.data$value),
    	.groups = "drop"
    ) |> 
    filter(!.data$variable %in% missing_variables) |> 
    pivot_wider(names_from = "variable", values_from = "coverage") |> 
    rename_with(
    	~ paste("annual", "coverage", .x, sep = "_"),
    	any_of(c("tmax", "tmin", "prcp"))
  )
  return (ans)
}

#' @title Calculate Period Coverage
#'
#' @description
#' `period_coverage()` calculates how many days have been recorded for 
#' the whole time period.
#'
#' @details
#' To calculate the coverage, a full daily time range is full joined to the
#' timeseries. Missing days are set to NA. Coverage is then calculated as
#' the number of values that are not NAs over the number of NAs.
#' Period coverage is a constant value for each station in the `ghcn_daily`
#' object.
#'
#' @importFrom dplyr mutate group_by full_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @return A table with period coverage.
#'
#' @examples
#' cleaned <- remove_flagged(CA003076680)
#' cover <- period_coverage(cleaned)
#' cover
period_coverage <- function(x) {
	stopifnot(inherits(x, "ghcn_daily"))
  .check_flags(x)
  x <- .drop_flags(x)

  timespan <- seq.Date(min(x$date), max(x$date), by = "day")

	x <- tibble(date = timespan) |> 
		full_join(x, by = "date", multiple = "all") |> 
    mutate(
    	year = as.numeric(format(date, "%Y")),
    	month = as.numeric(format(date, "%m"))
    ) |> 
    as_daily()

  missing_variables <- .missing_variables(x)
  if (length(missing_variables > 0)) x <- .add_variables(x)

  ans <- x |> 
    pivot_longer(
    	cols = c("tmax", "tmin", "prcp"),
    	names_to = "variable",
    	values_to = "value"
    ) |> 
    mutate(value = ifelse(.data$value == -9999, NA, .data$value)) |> 
    group_by(.data$variable) |> 
    summarize(
    	coverage = sum(!is.na(.data$value)) / length(.data$value),
    	.groups = "drop"
    ) |> 
    filter(!.data$variable %in% missing_variables) |> 
    pivot_wider(names_from = "variable", values_from = "coverage") |> 
    rename_with(
    	~ paste("period", "coverage", .x, sep = "_"),
    	any_of(c("tmax", "tmin", "prcp"))
  )
  return (ans)
}

#' @title Calculate Coverage of Daily Summaries
#'
#' @description
#' `coverage()` calculates the temporal coverage of the time series.
#' See also [monthly_coverage()], [annual_coverage()], and [period_coverage()].
#'
#' @details 
#' Returns a table with:
#' \itemize{
#'  \item mothly_coverage The proportion of the days with records in the month
#'  \item annual_coverage The proportion of the days with records in the year
#'  \item annual_coverage The proportion of the years with records in the reference period
#' }
#'
#' @importFrom dplyr mutate group_by group_split select distinct_all left_join bind_cols bind_rows
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains
#' @importFrom stats interaction.plot
#' @importFrom graphics grid
#' @importFrom rlang .data
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @param graph Logical, if to show a graph of annual coverage.
#'
#' @return A table with coverage.
#' @examples
#' cleaned <- remove_flagged(CA003076680)
#' cover <- coverage(cleaned)
#' cover[cover$month == 1, ]
coverage <- function(x, graph = FALSE) {
  stopifnot(inherits(x, "ghcn_daily"))
  .check_flags(x)
  x <- .drop_flags(x)

  # multiple stations: recursive, careful
  if (length(unique(x$station)) > 1) {
  	if (graph) message("Cannot plot coverage for more than one station.")
  	x_splitted <- x |> 
  	  group_by(.data$station) |> 
  	  group_split()
  	ans <- lapply(x_splitted, \(x) x |> as_daily() |> coverage(graph = FALSE) |> as_tibble())
  	ans <- bind_rows(ans)
  	return(ans)
  }

  ans <- x |> 
  	mutate(
  		year = as.numeric(format(.data$date, "%Y")),
  		month = as.numeric(format(.data$date, "%m")),
  	) |> 
    left_join(monthly_coverage(x), by = c("year", "month")) |> 
    left_join(annual_coverage(x), by = c("year")) |> 
    select("station", "year", "month", contains("coverage")) |> 
    distinct_all() |> 
    bind_cols(period_coverage(x)) |> 
    as_tibble()

  if (graph) {
  	with(
  		ans |> 
  			select("station", "year", contains("annual")) |> 
  			pivot_longer(cols = contains("coverage")) |> 
  			distinct_all() |> 
  			mutate(name = toupper(gsub("annual_coverage_", "", .data$name))),
  		interaction.plot(
  			year, name, value,
  			col = c("blue", "red", "gold"),
  			lty = 1,
  			trace.label = "",
  			las = 2,
  			xlab = "",
  			ylab = "Annual coverage",
  			xtick = TRUE,
  			xpd = FALSE,
  			main = ans$station[1]
  		)
  	)
  	grid()
  }

  return(ans)
}
