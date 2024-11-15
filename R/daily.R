#' @title Request to GET daily summaries
#'
#' @export
#' 
#' @param station_id Character vector of station id(s).
#' @param start_date Start date as character.
#' @param end_date Start date as character.
#'
#' @details dates should be given in `YYYY-mm-dd` format.
#' 
#' @return A tibble with the stations within the `roi`.
#' 
.daily_request <- function(station_id, start_date, end_date) {
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
    "includeAttributes=false&format=json"
  )
  return (req)
}



#' @title Download daily summaries
#'
#' @importFrom dplyr bind_rows mutate rename_with across group_by tally select
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom tidyselect matches
#'
#' @export
#' 
#' @param station_id Character vector of station id(s).
#' @param start_date Start date as character.
#' @param end_date Start date as character.
#'
#' @details dates should be given in `YYYY-mm-dd` format.
#' 
#' @return A tibble with the stations within the `roi`.
#' 
daily <- function(station_id, start_date, end_date) {
  req <- .daily_request(station_id, start_date, end_date)
  daily_data <- request(req)
  daily_data <- req_perform(daily_data)
  body <- resp_body_json(daily_data)
  if (length(body) == 0) {
    stop(" - No data found")
  }

  daily <- body |> 
    bind_rows() |> 
    mutate(across(!matches("DATE|STATION"), ~as.numeric(.x))) |> 
    rename_with(tolower) |> 
    mutate(date = as.POSIXct(.data$date))

  class(daily) <- c("daily", class(daily))

  return(daily)
}

#' @title Extract years from a POSIXct date.
#'
#' @export
#'
#' @param x POSIXct object.
.years <- function(x) {
  year <- sapply(x, \(x) strsplit(as.character(x), '-')[[1]][1])
  return (year)
}

#' @title Calculate Coverage of daily summaries
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
      year = .years(.data$date),
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

