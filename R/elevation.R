#' @title The GHCNd Station URL with Elevation
#' @return The URL of the GHCNd stations.
.elevation_url <- function() {
  return("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt")
}

#' @title Get GHCNd Station Elevation
#'
#' @importFrom readr read_fwf fwf_positions
#' @importFrom curl has_internet
#'
#' @export
#'
#' @return The table with the elevation of GHCNd stations.
#' 
#' @examples
#' \dontrun{
#' el <- elevation_stations()
#' }
elevation_stations <- function() {
  if (!has_internet()) stop("You do not have access to the internet. Check you connection and try again.")

  ans <- tryCatch(
    read_fwf(.elevation_url(),
      fwf_positions(
        start = c(1, 32),
        end = c(11, 38),
        col_names = c("station", "elevation")
      ),
      col_types = "cd"
    ),
    error = function(e) message(errorCondition(e))
  )
  if (is.null(ans)) stop(.elevation_url(), " not found.")

  return(ans)

}
