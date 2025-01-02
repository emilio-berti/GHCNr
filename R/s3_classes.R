#' @title Daily Class Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `ghcn_daily`.
#' @details Creates a new object of class `ghcn_daily`.
.s3_daily <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  structure(data, class = c("ghcn_daily", class(data)))
}

#' @title Cast Table to Daily
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select across
#' @importFrom tidyselect any_of
#' @importFrom rlang .data
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `ghcn_daily`.
#' @examples
#' \dontrun{
#' df <- read.csv(...)
#' df <- as_daily(df)
#' }
as_daily <- function(data) {
  if (inherits(data, "ghcn_daily")) {
    stop("Already a ghcn_daily object")
  }
  stopifnot(inherits(data, "data.frame"))
  ans <- data |>
    as_tibble() |>
    mutate(
      date = as.Date(.data$date, "%Y-%m-%d"),
      across(any_of(c("tmax", "tmin", "prcp")), ~as.numeric(.x))
    )
  ans <- .s3_daily(ans)
  return(ans)
}

#' @title Monthly Class Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `ghcn_monthly`.
#' @details Creates a new object of class `ghcn_monthly`.
.s3_monthly <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  structure(data, class = c("ghcn_monthly", class(data)))
}

#' @title Annual Class Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `ghcn_annual`.
#' @details Creates a new object of class `ghcn_annual`.
.s3_annual <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  structure(data, class = c("ghcn_annual", class(data)))
}

#' @title Annual Quarter Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `ghcn_quarterly`.
#' @details Creates a new object of class `ghcn_quarterly`.
.s3_quarterly <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  structure(data, class = c("ghcn_quarterly", class(data)))
}

#' @title Anomaly Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `ghcn_anomaly`.
#' @details Creates a new object of class `ghcn_anomaly`.
.s3_anomaly <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  structure(data, class = c("ghcn_anomaly", class(data)))
}
