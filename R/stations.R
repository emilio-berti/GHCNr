#' @title Get Stations for Area
#' 
#' @importFrom methods is
#' @importFrom terra vect plot points aggregate project crs relate xmin xmax ymin ymax
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

  # allows to recycle downloaded file if it was already downloaded
  # during this R session.
  if (is.null(getOption("stations_file"))) {
    message(" - Downloading stations inventory")
    inventory <- tempfile()
    download.file(
      "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt",
      inventory
    )
    options("stations_file" = inventory)
  } else {
    inventory <- getOption("stations_file")
  }
  
  stations <- read.table(
    inventory,
    header = FALSE,
    col.names = c("id", "lat", "lon", "dataType", "start", "end")
  ) |> 
    as_tibble()
  
  message(' - Filtering stations')
  # filter by bbox
  stations <- stations[stations$lon >= xmin(roi), ]
  stations <- stations[stations$lon <= xmax(roi), ]
  stations <- stations[stations$lat >= ymin(roi), ]
  stations <- stations[stations$lat <= ymax(roi), ]
  stations <- stations |> vect(crs = "EPSG:4326")  

  if (crs(stations) != crs(roi)) stations <- project(stations, roi)
  # s <- stations[relate(stations, roi, "within", pairs = FALSE)[, 1], ]
  stations <- mask(stations, roi)

  if (show) {
    plot(roi, col = "grey90")
    points(stations, col = "grey20", bg = "dodgerblue", pch = 21, cex = 1.5)    
  }

  return(stations)
}
