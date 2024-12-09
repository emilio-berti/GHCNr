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
		col_names = c("station", "longitude", "latitude", "variable", "startYear", "endYear"),
		show_col_types = FALSE
	) |> 
		filter(variable %in% toupper(variables))

	return(ans)
}
