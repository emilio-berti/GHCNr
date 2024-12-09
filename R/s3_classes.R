#' Daily Class Constructor
#'
#' This function creates a new object of class `daily`.
#' 
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `daily`.
#' @export
.s3_daily <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  # Add class attribute
  structure(data, class = c("ghcn-daily", class(data)))
}

#' Daily Class Constructor
#'
#' This function creates a new object of class `monthly`.
#' 
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `monthly`.
#' @export
.s3_monthly <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  # Add class attribute
  structure(data, class = c("ghcn-monthly", class(data)))
}

#' Daily Class Constructor
#'
#' This function creates a new object of class `annual`.
#' 
#' @param data A data frame or tibble to be used as the underlying data.
#' @return An object of class `annual`.
#' @export
.s3_annual <- function(data = tibble::tibble()) {
  if (!inherits(data, "tbl_df")) {
    stop("`data` must be a tibble or data frame")
  }
  # Add class attribute
  structure(data, class = c("ghcn-annual", class(data)))
}
