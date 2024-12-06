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
#' @param include_flags Logical, if to include flagged records.
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
  variables,
  include_flags
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
    "includeAttributes=", ifelse(include_flags, "true", "false"), "&",
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
#' @param remove_flagged Logical, if to remove flagged records.
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
  variables = c("tmax", "tmin", "prcp"),
  remove_flagged = TRUE
) {
  stopifnot(is.character(variables))
  stopifnot(is.logical(remove_flagged))

  req <- .daily_request(
    station_id,
    start_date,
    end_date,
    variables,
    include_flags = FALSE
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
    mutate(date = as.Date(date))

  if (remove_flagged) {
    req_flags <- .daily_request(
      station_id,
      start_date,
      end_date,
      variables,
      include_flags = TRUE
    )
    flags_data <- request(req_flags)
    flags_data <- req_perform(flags_data)
    flags_body <- resp_body_json(flags_data)

    daily_flags <- flags_body |> 
      bind_rows() |>
      rename_with(tolower) |>
      select(contains("attributes")) |>
      mutate(across(everything(), ~.extract_flag(.x)))

    daily_flags_indices <- matrix(
      as.matrix(daily_flags) %in% colnames(.flags()),
      ncol = ncol(daily_flags),
      byrow = FALSE
    )

    flagged_records <- sum(rowSums(daily_flags_indices) == 1)
    if (flagged_records > 0) {
      message(flagged_records, " flagged record(s):")
      table(unlist(.flags()[daily_flags[daily_flags_indices]]))
      daily_data <- daily_data[rowSums(daily_flags_indices) == 0, ]
    }

  }

  daily_data <- daily_data |> 
    select("date", "station", any_of(variables)) |>
    mutate(across(any_of(variables), ~as.numeric(.x)))

  return(daily_data)
}

#' @title Calculate Coverage of Daily Summaries
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
#'
daily_coverage <- function(x) {
  stopifnot(is(x, "daily"))
  coverage <- x |>
    mutate(
      year = format(.data$date, "%Y"),
      n_years = length(unique(.data$year))
    ) |>
    pivot_longer(cols = !matches("date|station|year|n_years")) |>
    group_by(.data$station, .data$name, .data$n_years) |>
    tally() |>
    ungroup() |>
    mutate(coverage = .data$n / 365 / .data$n_years) |>
    select("station", "name", "coverage") |>
    mutate(coverage = ifelse(.data$coverage > 1, 1, .data$coverage)) |>
    pivot_wider(names_from = "name", values_from = "coverage") |>
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))

  return(coverage)
}
