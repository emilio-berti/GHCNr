#' @title Download country shapefile from geoBoundaries
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom terra vect
#'
#' @export
#' 
#' @param country_code Three letter ISO code.
#' @param simplified Logical.
#'
#' @details <https://github.com/wmgeolab/geoBoundaries>.
#' 
#' @return A shapefile.
get_country <- function(
	country_code,
	simplified = TRUE
) {
	stopifnot(nchar(country_code) == 3)
	url <- paste(
		"https://www.geoboundaries.org/api/current/gbOpen",
		country_code,
		"ADM0",
		sep = "/"
	)
	response <- url |> 
		request() |> 
		req_perform() |>
		resp_body_json()

	if (simplified) {
		ans <- vect(response$simplifiedGeometryGeoJSON)
	} else {
		ans <- vect(response$gjDownloadURL)
	}

	return(ans)
}

#' @title Download multiple countries' shapefiles from geoBoundaries
#'
#' @importFrom terra vect
#'
#' @export
#'
#' @param countries_code Vector of three letter ISO code.
#' @param simplified Logical.
#'
#' @details <https://github.com/wmgeolab/geoBoundaries>.
#'
#' @return A shapefile.
get_countries <- function(
	countries_code,
	simplified = TRUE
) {
	ans <- lapply(countries_code, get_country, simplified)
	ans <- vect(ans)
	return(ans)
}
