#' @title Get Stations for Area
#' 
#' @importFrom methods is
#' @importFrom terra vect plot points aggregate project crs relate
#' @importFrom tibble as_tibble
#' @importFrom utils read.table download.file
#' 
#' @export
#' 
#' @param roi SpatVector defining the region of interest.
#' @param show Logical, show the stations on a map?
#' 
#' @return A tibble with the stations within the `roi`.
#' 
stations <- function(roi, show = FALSE) {
  stopifnot(is(roi, "SpatVector"))
  if (length(roi) > 1) roi <- aggregate(roi)

  message(" - Downloading stations inventory")
  inventory <- tempfile()
  download.file(
    "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt",
    inventory
  )
  
  stations <- read.table(
    inventory,
    header = FALSE,
    col.names = c("id", "lat", "lon", "dataType", "start", "end")
  ) |> 
    as_tibble() |> 
    vect(crs = "EPSG:4326")  

  if (crs(stations) != crs(roi)) stations <- project(stations, roi)
  s <- stations[relate(stations, roi, "within", pairs = FALSE)[, 1], ]

  if (show) {
    plot(roi, col = "grey90")
    points(s, col = "grey20", bg = "dodgerblue", pch = 21, cex = 1.5)    
  }

  return(s)
}
