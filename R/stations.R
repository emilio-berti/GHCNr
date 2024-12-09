#' @title The GHCNd Inventory URL
#'
#' @export
#'
#' @return A string of the URL.
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
#' @param filename Character of the filename of the inventory, if already downloaded.
#' @param variables Character, vector of the variables to include.
#'
#' @return A tibble with the station inventory.
#' 
#' @details
#' If \emph{filename} is not provided, this will download the inventory from
#' <"https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt">.
#' In alternative, you can download the invetory yourself and load it (see examples).
#' @examples
#' \dontrun{
#' dest <- tempfile()
#' download.file("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", dest)
#' s <- stations(dest)
#' }
stations <- function(filename, variables = c("tmin", "tmax", "prcp")) {
  if (missing(filename)) {
    src <- .inventory_url()
  } else{
    src <- filename
  }
  ans <- read_table(
    src,
    col_names = c("station", "latitude", "longitude", "variable", "startYear", "endYear"),
    show_col_types = FALSE
  ) |> 
    filter(variable %in% toupper(variables))

  return(ans)
}

#' @title Download GHCNd Inventory File
#'
#' @export
#'
#' @param filename Character of the filename of the inventory, if already downloaded.
#'
#' @return Character, the location of the file where the inventory has been saved.
#' 
#' @details
#' Download the inventory from <"https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt">.
download_inventory <- function(filename) {
  if (missing(filename)) stop("You need to specify a location for the inventory.")
  download.file("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", filename)  
  return(filename)
}

#' @title Spatial Filtering of Stations
#'
#' @importFrom terra vect relate crs geom
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate relocate pull
#'
#' @export
#'
#' @param stations, the table with station data.
#' @param roi, the geometry of the region of interest.
#'
#' @return Table with filtered stations.
filter_stations <- function(stations, roi) {
  stopifnot("longitude" %in% colnames(stations))
  stopifnot("latitude" %in% colnames(stations))

  stations <- vect(
    stations,
    geom = c("longitude", "latitude"), 
    crs = crs("EPSG:4326")
  )
  stations <- mask(stations, italy)
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
    relocate(latitude, .after = "station") |> 
    relocate(longitude, .after = "latitude")

  return(ans)
}
