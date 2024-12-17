#' @title Create Request URL for Daily Summaries
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
.daily_url <- function(
  station_id,
  start_date,
  end_date,
  variables
) {
  if (nchar(strsplit(start_date, "-")[[1]][1]) < 4) {
    warning("Date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(start_date, "-")[[1]][2]) > 12) {
    warning("Date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(start_date, "-")[[1]][3]) > 31) {
    warning("Date format should be YYYY-MM-DD")
  }
  if (nchar(strsplit(end_date, "-")[[1]][1]) < 4) {
    warning("Date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(end_date, "-")[[1]][2]) > 12) {
    warning("Date format should be YYYY-MM-DD")
  }
  if (as.numeric(strsplit(end_date, "-")[[1]][3]) > 31) {
    warning("Date format should be YYYY-MM-DD")
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

#' @title Request Daily Summaries
#' @importFrom httr2 request req_user_agent req_perform resp_body_json last_response resp_status
#' @param url Character, URL of the request.
#' @return Body of the JSON request.
.daily_request <- function(url) {
  req <- url |>
    request() |>
    req_user_agent("GHCNr (https://cran.r-project.org/package=GHCNr)")

  # try to get response and handle errors
  resp <- tryCatch(
    req |> req_perform() |> resp_body_json(),
    error = function(e) NULL
  )

  # API error
  if (is.null(resp)) .api_error(last_response())  

  # no error, but no data found
  if (length(resp) == 0 && (last_response() |> resp_status() < 400)) {
    message("No data found.")
    return(NULL)
  }

  return(resp)
}

#' @title Download Daily Summaries
#'
#' @importFrom dplyr bind_rows mutate rename_with across group_by tally select
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
#'
#' @examples
#' \dontrun{
#' CA003076680 <- daily("CA003076680", "1990-01-01", "2024-12-31") 
#' }
daily <- function(
  station_id,
  start_date,
  end_date,
  variables = c("tmax", "tmin", "prcp")
) {
  stopifnot(is.character(variables))

  url <- .daily_url(
    station_id,
    start_date,
    end_date,
    variables
  )
  body <- .daily_request(url)
  stopifnot(!is.null(body))

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

  ans <- daily_data |> 
    select("date", "station", any_of(tolower(variables)), contains("flag")) |>
    mutate(across(any_of(tolower(variables)), ~as.numeric(.x)))

  ans <- .s3_daily(ans)

  return(ans)
  
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
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @param strict Logical, if to remove also `looser` flags.
#'
#' @return `x` without flagged records.
#'
#' @examples
#' remove_flagged(CA003076680)
remove_flagged <- function(x, strict = TRUE) {
  stopifnot(inherits(x, "ghcn_daily"))

  flagged <- matrix(
    as.matrix(x |> select(contains("flag"))) %in% colnames(.flags(strict)),
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
    flags <- flags[names(flags) %in% colnames(.flags(strict))]
    for (i in seq_along(flags)) {
      message(" - ", flags[i], " ", .flags(strict)[names(flags)[i]], "(s)")
    }
    x <- x[rowSums(flagged) == 0, ]
  } else {
    message("No flagged records found.")
  }
  x <- x |> select(-contains("flag"))

  return (x)
}
