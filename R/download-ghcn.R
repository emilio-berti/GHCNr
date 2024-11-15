# #' @title Download data from GHCN
# #'
# #' @export
# #' 
# #' @param station_id Character vector of station id(s).
# #' @param start_date Start date as character.
# #' @param end_date Start date as character.
# #'
# #' @details dates should be given in `YYYY-mm-dd` format.
# #' 
# #' @return A tibble with the stations within the `roi`.
# get_ghcn <- function(
#   station_id = NULL,
#   roi = NULL,
#   variable,
#   start_date,
#   end_date
# ) {
#   if (all(is.null(station_id)) && is.null(roi)) {
#     stop("Provide either a station_id or a roi.")
#   }
# s <- stations(roi, show = FALSE)
# s <- s[s$dataType == "TMAX", ]
# s <- mask(s, germany)

# plot(germany)
# points(s[1:5])
# d <- daily(s$id[1:10], "1979-01-01", "2023-12-31")
# daily_coverage(d)



# d <- d |>
#   left_join(
#     geom(s) |> as_tibble() |> select(x, y) |> mutate(station = s$id)
#   ) |>
#   mutate(longitude = x, latitude = y) |>
#   select(-x, -y)



# with(
#   subset(d, station == unique(d$station)[2]),
#   plot(
#     date,
#     tmax / 10,
#     col = "violet",
#     type = "l",
#     frame = FALSE,
#     xlab = "Date",
#     ylab = "Maximum temperature"
#   )
# )
# with(
#   subset(d, station == unique(d$station)[5]),
#   lines(date, tmax / 10, col = "blue")
# )
# with(
#   subset(d, station == unique(d$station)[4]),
#   lines(date, tmax / 10, col = "green2")
# )
# with(
#   subset(d, station == unique(d$station)[3]),
#   lines(date, tmax / 10, col = "gold2")
# )
# with(
#   subset(d, station == unique(d$station)[1]),
#   lines(date, tmax / 10, col = "tomato2")
# )
# ```

# # focus on 3 stations only
# points(
#   s[7:9],
#   col = "grey20", bg = c("violet", "tomato2", "gold"),
#   pch = 23, cex = 2
# )
# d <- daily(unique(s$id)[7:9], "1974-01-01", "2023-12-31")
# daily_coverage(d)
# with(
#   subset(d, station == unique(d$station)[1]),
#   plot(
#     date,
#     tmax / 10,
#     col = "violet",
#     type = "l",
#     frame = FALSE,
#     xlab = "Date",
#     ylab = "Maximum temperature"
#   )
# )
# with(
#   subset(d, station == unique(d$station)[2]),
#   lines(date, tmax / 10, col = "tomato2")
# )
# with(
#   subset(d, station == unique(d$station)[3]),
#   lines(date, tmax / 10, col = "gold2")
# )

# }
