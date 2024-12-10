#' @title Calculate Monthly Summaries
#'
#' @importFrom dplyr select mutate across distinct_all summarize group_by left_join
#' @importFrom tidyselect contains everything all_of
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#'
#' @export
#'
#' @param x Table of the daily timeseries.
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

  if (any(grepl("flag", colnames(x)))) {
    flags <- x |> 
      select(contains("flag")) |> 
      mutate(across(everything(), ~ifelse(.x == "", NA, .x))) |> 
      drop_na()
    if (nrow(flags >= 1)) warning("Flags found. Consider removing flagged records, e.g. remove_flagged")
    x <- x |> select(-contains("flag"))
  }

  # add variables to not break summarize
  if (!all(c("tavg", "tmin", "tmax", "prcp") %in% colnames(x))) {
    missing_variable <- setdiff(c("tavg", "tmin", "tmax", "prcp"), colnames(x))
    for (v in missing_variable) {
      x[[v]] <- -9999
    }
  }

  ans <- x |>
    mutate(
      year = format(.data$date, "%Y"),
      month = format(.data$date, "%m")
    ) |> 
    group_by(.data$station, .data$year, .data$month) |>
    summarize(
      tmin = .min(.data$tmin),
      tmax = .max(.data$tmax),
      tavg = .mean(.data$tavg),
      prcp = .sum(.data$prcp),
      .groups = "drop"
    ) |>
    select(-all_of(missing_variable)) |> 
    mutate(
      year = as.numeric(.data$year),
      month = as.numeric(.data$month)
    )
  
  ans <- ans |> 
    left_join(
      coverage(x) |> select("station", "monthly_coverage", "year", "month"),
      by = c("station", "year", "month")
    )

  ans <- .s3_monthly(ans)

  return(ans)
}
