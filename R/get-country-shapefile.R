#' @title Download country shapefile from geoBoundaries
#'
#' @importFrom httr2 request req_user_agent req_perform resp_body_json last_response resp_status
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
#' @examples
#' \dontrun{
#' italy <- get_country("ITA")
#' }
get_country <- function(
  country_code,
  simplified = TRUE
) {
  stopifnot(nchar(country_code) == 3)
  if (length(country_code) > 1) {
    stop("Too many country codes. Use country_codes() for multiple countries.")
  }
  url <- paste(
    "https://www.geoboundaries.org/api/current/gbOpen",
    country_code,
    "ADM0",
    sep = "/"
  )
  req <- url |>
    request() |>
    req_user_agent("GHCNr (https://cran.r-project.org/package=GHCNr)")

  # try to get response and handle errors
  resp <- tryCatch(
    req |> req_perform() |> resp_body_json(),
    error = function(e) NULL
  )

  # API error
  if (is.null(resp)) {
    .api_error(last_response())
  }

  # no error, but no data found
  if (length(resp) == 0 && (last_response() |> resp_status() < 400)) {
    stop("No data found.")
  }

  stopifnot(!is.null(resp))

  if (simplified) {
    ans <- vect(resp$simplifiedGeometryGeoJSON)
  } else {
    ans <- vect(resp$gjDownloadURL)
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
#'
#' @examples
#' \dontrun{
#' eu <- get_countries(country_code$iso3, simplified = TRUE)
#' }
get_countries <- function(
  countries_code,
  simplified = TRUE
) {
  ans <- lapply(countries_code, get_country, simplified)
  ans <- vect(ans)
  return(ans)
}
