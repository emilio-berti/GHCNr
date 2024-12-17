#' @title The GHCNd Inventory URL
#' @return The URL of the GHCNd inventory.
.inventory_url <- function() {
  return("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt")
}

#' @title Get GHCNd Inventory
#'
#' @importFrom readr read_table
#' @importFrom dplyr filter
#'
#' @export
#'
#' @param filename Character, the filename of the inventory, if already downloaded.
#' @param variables Character, vector of the variables to include.
#' @param first_year Integer, the year since when data should be recorded.
#' @param last_year Integer, the year until when data should be recorded.
#'
#' @return The table with the GHCNd stations.
#' 
#' @details
#' If \emph{filename} is not provided, this will download the inventory from
#' <"https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt">.
#' In alternative, you can download the invetory yourself and load it (see examples).
#' @examples
#' \dontrun{
#' dest <- tempfile()
#' download_inventory(dest)
#' s <- stations(dest)
#' }
stations <- function(
  filename,
  variables = c("tmin", "tmax", "prcp"),
  first_year,
  last_year
) {
  if (missing(filename)) {
    src <- .inventory_url()
  } else{
    src <- filename
  }
  ans <- tryCatch(
    read_table(
      src,
      col_names = c("station", "latitude", "longitude", "variable", "startYear", "endYear"),
      show_col_types = FALSE
    ) |> 
      filter(.data$variable %in% toupper(variables)),
    error = function(e) message(errorCondition(e))
  )
  if (is.null(ans)) stop(.inventory_url(), " not found.")

  if (!missing(first_year)) {
    ans <- ans |>filter(.data$startYear <= first_year)
  }
  if (!missing(last_year)) {
    ans <- ans |>filter(.data$endYear >= last_year)
  }

  return(ans)
}

#' @title Download GHCNd Inventory File
#'
#' @importFrom utils download.file
#'
#' @export
#'
#' @param filename Character of the filename of the inventory, if already downloaded.
#'
#' @return Character, the location of the file where the inventory has been saved.
#' 
#' @details
#' Download the inventory from <"https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt">.
#' @examples
#' \dontrun{
#' download_inventory(...)
#' }
download_inventory <- function(filename) {
  if (missing(filename)) stop("You need to specify a location for the inventory.")
  download.file(.inventory_url(), filename)  
  return(filename)
}

#' @title Spatial Filtering of Stations
#'
#' @importFrom terra vect relate crs geom mask
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate relocate pull
#'
#' @export
#'
#' @param stations, the table with station data. See [stations()].
#' @param roi, the geometry of the region of interest. See [get_country()].
#'
#' @return Table with filtered stations.
#'
#' @examples
#' \dontrun{
#' inventory <- stations()
#' roi <- get_country("ITA")
#' s <- filter_stations(inventory, roi)
#' }
filter_stations <- function(stations, roi) {
  stopifnot("longitude" %in% colnames(stations))
  stopifnot("latitude" %in% colnames(stations))
  stopifnot(inherits(roi, "SpatVector"))

  stations <- vect(
    stations,
    geom = c("longitude", "latitude"), 
    crs = crs("EPSG:4326")
  )
  stations <- mask(stations, roi)
  # NOTE: check if relate within is the same as mask()
  # within <- relate(stations, roi, "within")  
  # within <- rowSums(within) >= 1
  # stations <- stations[within, ]
  ans <- stations |> 
    as_tibble() |> 
    mutate(
      longitude = stations |> geom() |> as_tibble() |> pull("x"),
      latitude = stations |> geom() |> as_tibble() |> pull("y")
    ) |> 
    relocate(.data$latitude, .after = "station") |> 
    relocate(.data$longitude, .after = "latitude")

  return(ans)
}
