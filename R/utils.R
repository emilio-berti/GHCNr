#' @title Number of Days in Month
#' @importFrom tibble tibble
#' @export
#' @return Table with number of days in the months.
.days_in_month <- function() {
  ans <- tibble(
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") ,
    days = c(  31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  )
  return(ans)
}

#' @title Check Ahich Variables Are Absent 
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