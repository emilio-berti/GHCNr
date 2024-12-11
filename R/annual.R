#' @title Calculate Annual Summaries
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
#' \emph{x} is the table returned from \code{get_ghcn_daily()} or 
#' \code{remove_flagged()} or any subset of them.
#'
#' @return A tibble with the annual timeseries at the stations.
#' @examples
#' annual(CA003076680)
annual <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))

  x <- .drop_flags(x)
  missing_variable <- .missing_variables(x)
  x <- .add_variables(x)

  ans <- x |>
    mutate(year = format(.data$date, "%Y")) |> 
    group_by(.data$station, .data$year) |>
    summarize(
      tmin = .min(.data$tmin),
      tmax = .max(.data$tmax),
      tavg = .mean(.data$tavg),
      prcp = .sum(.data$prcp),
      .groups = "drop"
    ) |>
    select(-all_of(missing_variable)) |> 
    mutate(year = as.numeric(.data$year))
  
  ans <- ans |> 
    left_join(
      coverage(x) |> 
        select("station", "annual_coverage", "year") |> 
        distinct_all(),
      by = c("station", "year")
    )

  ans <- .s3_annual(ans)
  
  return(ans)
}
