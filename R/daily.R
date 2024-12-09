#' @title GHCNd Flags
#'
#' @export
#'
#' @details <https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>
#' @return Table with flags.
.flags <- function() {
  ans <- data.frame(
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

#' @title Create Request URL for Daily Summaries
#'
#' @export
#'
#' @param station_id Character, station id(s).
#' @param start_date Character, start date.
#' @param end_date Character, end date.
#' @param variables Character, vector of the variables to include.
#'
#' @details
#' \emph{station_id} can be a vector with multiple stations.
#' Dates should be given in `YYYY-mm-dd` format.
#'
#' @return Character string with the API URL.
.daily_request <- function(
  station_id,
  start_date,
  end_date,
  variables
) {
  if (nchar(strsplit(start_date, "-")[[1]][1]) < 4) {
    warning("   date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(start_date, "-")[[1]][2]) > 12) {
    warning("   date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(start_date, "-")[[1]][3]) > 31) {
    warning("   date format should be YYYY-MM-DD")
  }
  if (nchar(strsplit(end_date, "-")[[1]][1]) < 4) {
    warning("   date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(end_date, "-")[[1]][2]) > 12) {
    warning("   date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(end_date, "-")[[1]][3]) > 31) {
    warning("   date format should be YYYY-MM-DD")
  }

  if (length(station_id) > 1) station_id <- paste(station_id, collapse = ",")
  req <- paste0(
    "https://www.ncei.noaa.gov/access/services/data/v1?",
    "dataset=daily-summaries&",
    "stations=", station_id, "&",
    "startDate=", start_date, "&",
    "endDate=", end_date, "&",
    "units=metric", "&",
    "dataTypes=", paste(toupper(variables), collapse = ","), "&",
    "includeAttributes=true&",
    "format=json"
  )
  return(req)
}

#' @title Download Daily Summaries
#'
#' @importFrom dplyr bind_rows mutate rename_with across group_by tally select
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom tidyselect matches contains everything any_of
#' @importFrom rlang .data
#'
#' @export
#'
#' @param station_id Character, station id(s).
#' @param start_date Character, start date.
#' @param end_date Character, end date.
#' @param variables Character, vector of the variables to include.
#'
#' @details
#' \emph{station_id} can be a vector with multiple stations.
#' Dates should be given in `YYYY-mm-dd` format.
#' Available \emph{variables} can be found at <https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt>.
#'
#' @return A tibble with the daily timeseries at the stations.
daily <- function(
  station_id,
  start_date,
  end_date,
  variables = c("tmax", "tmin", "prcp")
) {
  stopifnot(is.character(variables))

  req <- .daily_request(
    station_id,
    start_date,
    end_date,
    variables
  )
  daily_data <- request(req)
  daily_data <- req_perform(daily_data)
  body <- resp_body_json(daily_data)

  if (length(body) == 0) {
    stop("No data found")
  }

  daily_data <- body |> 
    bind_rows() |>
    rename_with(tolower) |>
    mutate(
      date = as.Date(date),
      across(contains("attributes"), ~.extract_flag(.x))
    ) |> 
    rename_with(
      ~gsub("attributes", "flag", .x),
      contains("attributes")
    )

  daily_data <- daily_data |> 
    select("date", "station", any_of(tolower(variables)), contains("flag")) |>
    mutate(across(any_of(tolower(variables)), ~as.numeric(.x)))

  return(daily_data)
  
}

#' @title Remove Flagged Recrods
#'
#' @importFrom dplyr mutate group_by tally ungroup select across
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na
#' @importFrom tidyselect where matches
#' @importFrom rlang .data
#'
#' @export
#'
#' @param x Object of class `daily`.
#'
#' @details dates should be given in `YYYY-mm-dd` format.
#'
#' @return A tibble with the stations within the `roi`.
remove_flagged <- function(x) {
  flagged <- matrix(
    as.matrix(x |> select(contains("flag"))) %in% colnames(.flags()),
    ncol = ncol(x |> select(contains("flag"))),
    byrow = FALSE
  )
  n_flagged <- sum(rowSums(flagged) >= 1)
  if (n_flagged > 0) {
    message("Removing ", n_flagged, " flagged record(s):")
    flags <- x |> 
      select(contains("flag")) |> 
      unlist() |> 
      table()
    flags <- flags[names(flags) %in% colnames(.flags())]
    for (i in seq_along(flags)) {
      message(" - ", flags[i], " ", .flags()[names(flags)[i]], "(s)")
    }
    x <- x[rowSums(flagged) == 0, ]
    x <- x |> select(-contains("flag"))
  } else {
    message("No flagged records found.")
  }
  return (x)
}

#' @title Calculate Coverage of Daily Summaries
#'
#' @importFrom dplyr mutate group_by tally add_tally ungroup select across distinct_all
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na
#' @importFrom tidyselect where matches any_of
#' @importFrom rlang .data
#'
#' @export
#'
#' @param x Object of class `daily`.
#'
#' @details dates should be given in `YYYY-mm-dd` format.
#'
#' @return A tibble with the stations within the `roi`.
daily_coverage <- function(x) {
  days_in_month <- tibble(
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") ,
    days = c(  31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  )
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
    left_join(days_in_month, by = "month") |> 
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
