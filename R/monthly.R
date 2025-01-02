#' @title Calculate Monthly Summaries
#'
#' @importFrom dplyr select mutate summarize group_by
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#'
#' @export
#'
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#'
#' @details
#' \emph{x} is the table returned from \code{daily()} or 
#' \code{remove_flagged()} or any subset of them.
#'
#' @return A tibble with the monthly timeseries at the stations.
#'
#' @examples
#' monthly(CA003076680)
monthly <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))

  .check_flags(x)
  x <- .drop_flags(x)
  missing_variable <- .missing_variables(x)
  x <- .add_variables(x)

  ans <- x |>
    mutate(
      year = format(.data$date, "%Y"),
      month = format(.data$date, "%m")
    ) |> 
    group_by(.data$station, .data$year, .data$month) |>
    summarize(
      tmin = .min(.data$tmin),
      tmax = .max(.data$tmax),
      prcp = .sum(.data$prcp),
      .groups = "drop"
    ) |>
    select(-all_of(missing_variable)) |> 
    mutate(
      year = as.numeric(.data$year),
      month = as.numeric(.data$month)
    )
  
  ans <- .s3_monthly(ans)

  return(ans)
}
