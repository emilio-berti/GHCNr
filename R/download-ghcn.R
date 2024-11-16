#' @title Download data from GHCN
#'
#' @importFrom dplyr select mutate left_join all_of
#' @importFrom tibble as_tibble
#' @importFrom terra mask geom
#'
#' @export
#' 
#' @param country_code 3 letter ISO-3 country code.
#' @param variable either TMAX, TMIN, or TAVG.
#' @param start_date Start date as character (%YYYY-%mm-%dd).
#' @param end_date Start date as character (%YYYY-%mm-%dd).
#'
#' @details 
#' Dates should be given in `YYYY-mm-dd` format.
#' A list of ISO-3 for Europe is in GHCNr::europe_codes.
#' 
#' @return A tibble with the stations within the `roi`.
get_daily_weather <- function(
  country_code,
  variable,
  start_date,
  end_date
) {
	roi <- get_country(country_code)
  s <- stations(roi)
	s <- s[s$dataType == variable, ]
	s <- mask(s, roi)

	if (length(s$id) >= 1e2) {
		chunks <- ceiling(length(s$id) / 1e2)
		ans <- as.list(rep(NA, chunks))
		for (i in seq_len(chunks)) {
			ids <- s$id[seq( (i - 1) * 100 + 1, i * 100)]
			ans[[i]] <- daily(ids, start_date, end_date) |>
			  left_join(  # add spatial coordinates
			    s |> 
			    	geom() |>
			    	as_tibble() |>
			    	select("x", "y") |>
			    	mutate(station = s$id),
			    by = "station"
			  ) |>
			  mutate(  # flatten date
			  	longitude = .data$x,
			  	latitude = .data$y,
			  	year = as.numeric(format(date, "%Y")),
				 	month = as.numeric(format(date, "%m")),
				 	day = as.numeric(format(date, "%d"))
			  ) |>
			  select(  # select features
			  	"date", "year", "month", "day", 
			  	"station", "longitude", "latitude",
			  	all_of(tolower(variable))
			  )
			}
			ans <- ans |> bind_rows()
		} else {
			ans <- daily(s$id, start_date, end_date) |>
			  left_join(  # add spatial coordinates
			    s |> 
			    	geom() |>
			    	as_tibble() |>
			    	select("x", "y") |>
			    	mutate(station = s$id),
			    by = "station"
			  ) |>
			  mutate(  # flatten date
			  	longitude = .data$x,
			  	latitude = .data$y,
			  	year = as.numeric(format(date, "%Y")),
				 	month = as.numeric(format(date, "%m")),
				 	day = as.numeric(format(date, "%d"))
			  ) |>
			  select(  # select features
			  	"date", "year", "month", "day", 
			  	"station", "longitude", "latitude",
			  	all_of(tolower(variable))
			  )
	}
	if (sum(!unique(s$id) %in% unique(ans$station)) > 0) {
		message(
			"No data found for ",
			sum(!unique(s$id) %in% unique(ans$station)),
			" of the ", length(unique(s$id)), " stations available."
		)
	}
	return(ans)
}
