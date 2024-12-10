#' @title Daily Class Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `daily`.
#' @details Creates a new object of class `daily`.
.s3_daily <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  # Add class attribute
  structure(data, class = c("ghcn_daily", class(data)))
}

#' @title Cast Table to Daily
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select
#' @importFrom rlang .data
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `daily`.
#' @examples
#' \dontrun{
#' df <- read.csv(...)
#' df <- as_daily(df)
#' }
as_daily <- function(data) {
  if (inherits(ans, "ghcn_daily")) {
    stop("Already a ghcn_daily object")
  }
  stopifnot(inherits(data, "data.frame"))
  ans <- data |>
    as_tibble() |>
    mutate(date = as.Date(.data$date, "%Y-%m-%d"))
  ans <- .s3_daily(ans)
  return(ans)
}

#' @title Daily Class Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `monthly`.
#' @details Creates a new object of class `monthly`.
.s3_monthly <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  # Add class attribute
  structure(data, class = c("ghcn_monthly", class(data)))
}

#' @title Daily Class Constructor
#' @export
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `annual`.
#' @details Creates a new object of class `annual`.
.s3_annual <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  # Add class attribute
  structure(data, class = c("ghcn_annual", class(data)))
}
