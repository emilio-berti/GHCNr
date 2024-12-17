#' @title Handles API Errors
#' @importFrom httr2 resp_status resp_status_desc
#' @export
#' @param resp Object of class `httr2_response`.
#' @return NULL, called for side effects.
.api_error <- function(resp) {
  stopifnot(inherits(resp, "httr2_response"))
  status_code <- resp |> resp_status()
  message(
    paste(
      "API error:",
      status_code,
      resp |> resp_status_desc()
    )
  )
  if (status_code == 414) {
    message("URI is too long. Try asking for data from fewer stations.")
  }
  if (status_code == 408 || status_code >= 500) {
    message("Retry in few moments.")
  }
}

#' @title Check Which Variables Are Absent 
#' @importFrom tibble tibble
#' @export
#' @param x Object of class `ghcn_daily`.
#' @return Character vector
.missing_variables <- function(x) {
  stopifnot(inherits(x, "ghcn_daily") || inherits(x, "ghcn_monthly") || inherits(x, "ghcn_annual"))
  ans <- setdiff(c("tmin", "tmax", "prcp"), colnames(x))
  return(ans)
}

#' @title Add Columns to Handle Summarize
#' @importFrom tibble tibble
#' @export
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @return Table with number of days in the months.
.add_variables <- function(x) {
  stopifnot(inherits(x, "ghcn_daily") || inherits(x, "ghcn_monthly") || inherits(x, "ghcn_annual"))

  # add variables to not break summarize
  if (!all(c("tmin", "tmax", "prcp") %in% colnames(x))) {
    missing_variables <- .missing_variables(x)
    for (v in missing_variables) {
      x[[v]] <- -9999
    }
  }
  return(x)
}

#' @title Calculate Minimum
#' @export
#' @param x Numeric vector
#' @return Numeric.
.min <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, min(x), NA)
    return(ans)
}
#' @title Calculate Maximum
#' @export
#' @param x Numeric vector
#' @return Numeric.
.max <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, max(x), NA)
    return(ans)
}
#' @title Calculate Mean
#' @export
#' @param x Numeric vector
#' @return Numeric.
.mean <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, mean(x), NA)
    return(ans)
}
#' @title Calculate Sum
#' @export
#' @param x Numeric vector
#' @return Numeric.
.sum <- function(x) {
    x <- x[!is.na(x)]
    ans <- ifelse(length(x) > 0, sum(x), NA)
    return(ans)
}
