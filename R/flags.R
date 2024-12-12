#' @title GHCNd Flags
#' @importFrom tibble tibble
#' @export
#' @param strict Logical, if to include all flags (TRUE) or not (FALSE).
#' @details <doi:10.1175/2010JAMC2375.1>
#' @return Table with flags.
.flags <- function(strict) {
  ans <- tibble(
    "D" = "duplicate flag",
    "I" = "consistency flag",
    "K" = "streak flag",
    "M" = "mega flag",
    "N" = "naught flag",
    "R" = "lagged range flag",
    "X" = "bounds flag"
  )
  if (strict) {
  ans <- ans |>
    mutate(
      "O" = "outlier flag",
      "G" = "gap flag", 
      "L" = "multiday flag",
      "S" = "spatial consistency flag",
      "Z" = "Datzilla flag"
    )
  }
  return(ans)
}

#' @title Extract GHCNd Flags
#' @export
#' @param x Character, vector of the flag as returned by the GHCNd API call.
#' @details <https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>
#' @return Character of the flag.
.extract_flag <- function(x) {
  x <- sapply(x, \(x) ifelse(is.na(x), ",,,", x))
  x <- sapply(x, \(x) strsplit(x, ",")[[1]][2])
  return(x)
}

#' @title Drop Flags Columns
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyselect contains everything
#' @importFrom tidyr drop_na
#' @export
#' @param x Object of class ghcn_daily.
#' @return The original objects without flags column.
.drop_flags <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))
  if (any(grepl("flag", colnames(x)))) {
    flags <- x |> 
      select(contains("flag")) |> 
      mutate(across(everything(), ~ifelse(.x == "", NA, .x))) |> 
      drop_na()
    if (nrow(flags >= 1)) warning("Flags found. Consider removing flagged records, e.g. remove_flagged")
    x <- x |> select(-contains("flag"))
  }
  return(x)
}

#' @title Check Flags Columns
#'
#' @importFrom dplyr select distinct_all
#' @importFrom tidyselect contains
#' @export
#' @param x Object of class ghcn_daily.
#' @return NULL, called for side effects
.check_flags <- function(x) {
  stopifnot(inherits(x, "ghcn_daily"))
  if (any(grepl("flag", colnames(x)))) {
    flagged <- x |> 
      select(contains("flag")) |> 
      distinct_all() |> 
      unlist() |> 
      unique()
    if (any(colnames(GHCNr::.flags(strict = TRUE)) %in% flagged)) {
      warning("Flags found, considering dropping flagged records. See ?remove_flagged() for details.")
    }
  }
}
