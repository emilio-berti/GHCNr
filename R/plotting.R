#' @title Plot GHCN Timeseries
#' @param x Object of class ghcn_daily.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to \code{plot()}.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @examples
#' plot(CA003076680, "tmax")
plot.ghcn_daily <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_daily"))
  stopifnot(variable %in% c("tmin", "tmax", "prcp"))
  op <- par(no.readonly = TRUE)
  par(mar = c(8, 8, 1, 1))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Roma")
  }
  names(palette) <- unique(x[["station"]])
  plot(
    x$date, x[[variable]],
    xlab = "",
    ylab = toupper(variable),
    col = palette[x[["station"]]],
    pch = 3,
    type = "n",
    frame = FALSE,
    axes = FALSE,
    ...
  )
  axis.Date(
    1,
    at = seq(min(x$date), max(x$date) + 180, by = 90),
    format = "%Y-%m-%d",
    las = 2
  )
  yrange <- range(x[[variable]], na.rm = TRUE)
  axis(
    2,
    at = round(seq(floor(yrange[1]), ceiling(yrange[2]), length.out = 5))
  )
  for (s in unique(x[["station"]])) {
    lines(
      x$date[x$station == s], x[[variable]][x$station == s],
      col = palette[s]
    )
  }
  par(op)
}

#' @title Plot GHCN Timeseries
#' @param x Object of class ghcn_monthly.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to \code{plot()}.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @examples
#' plot(monthly(CA003076680), "tmax")
plot.ghcn_monthly <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_monthly"))
  stopifnot(variable %in% c("tmin", "tmax", "prcp"))
  op <- par(no.readonly = TRUE)
  par(mar = c(8, 8, 1, 1))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Roma")
  }
  names(palette) <- unique(x[["station"]])
  years <- range(x[["year"]])
  x$date <- as.Date(paste(x$year, x$month, "01", sep = "-"))
  plot(
    x$date, x[[variable]],
    xlab = "",
    ylab = toupper(variable),
    col = palette[x[["station"]]],
    pch = 3,
    type = "n",
    frame = FALSE,
    axes = FALSE,
    ...
  )
  axis.Date(
    1,
    at = seq(min(x$date), max(x$date) + 180, by = 90),
    format = "%Y-%m-%d",
    las = 2
  )
  yrange <- range(x[[variable]], na.rm = TRUE)
  axis(
    2,
    at = round(seq(floor(yrange[1]), ceiling(yrange[2]), length.out = 5))
  )
  for (s in unique(x[["station"]])) {
    lines(
      x$date[x$station == s], x[[variable]][x$station == s],
      col = palette[s],
      lw = 2
    )
  }
  par(op)
}

#' @title Plot GHCN Timeseries
#' @param x Object of class ghcn_annual.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to \code{plot()}.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @examples
#' plot(annual(CA003076680), "tmax")
plot.ghcn_annual <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_annual"))
  stopifnot(variable %in% c("tmin", "tmax", "prcp"))
  op <- par(no.readonly = TRUE)
  par(mar = c(8, 8, 1, 1))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Roma")
  }
  names(palette) <- unique(x[["station"]])
  years <- range(x[["year"]])
  plot(
    x$year, x[[variable]],
    xlab = "",
    ylab = toupper(variable),
    col = palette[x[["station"]]],
    pch = 3,
    type = "n",
    frame = FALSE,
    axes = FALSE,
    ...
  )
  axis(1, at = seq(years[1], years[2], by = 1), las = 2)
  yrange <- range(x[[variable]], na.rm = TRUE)
  axis(
    2,
    at = round(seq(floor(yrange[1]), ceiling(yrange[2]), length.out = 5))
  )
  for (s in unique(x[["station"]])) {
    lines(
      x$year[x$station == s], x[[variable]][x$station == s],
      col = palette[s],
      lw = 3
    )
  }
  par(op)
}
