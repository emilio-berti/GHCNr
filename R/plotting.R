#' @title Plot GHCN Timeseries
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to [stats::interaction.plot()].
#' @importFrom grDevices hcl.colors
#' @importFrom stats interaction.plot
#' @export
#' @return NULL, called for side effects.
#' @examples
#' plot(CA003076680, "tmax")
plot.ghcn_daily <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_daily"))
  
  args <- list(...)
  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Viridis")
  }
  names(palette) <- unique(x[["station"]])
  
  if (missing(variable)) {
    variables <- intersect(c("tmax", "tmin", "prcp"), colnames(x))
    stopifnot(length(variables) > 0)
    for (v in variables) {
      interaction.plot(
        x.factor = x[["date"]],
        trace.factor = x[["station"]],
        response = x[[v]],
        trace.label = "Station",
        lty = 1,
        col = palette,
        xlab = "",
        ylab = toupper(v),
        ...
      )
      if (interactive() && v != variables[length(variables)]) {
        readline("Press any key to show the next variable")
      }
    }
  } else {
    stopifnot(variable %in% colnames(x))
    interaction.plot(
      x.factor = x[["date"]],
      trace.factor = x[["station"]],
      response = x[[variable]],
      trace.label = "Station",
      lty = 1,
      col = palette,
      xlab = "",
      ylab = toupper(variable),
      ...
    )
  }
}

#' @title Plot GHCN Timeseries
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to [stats::interaction.plot()].
#' @importFrom grDevices hcl.colors
#' @importFrom stats interaction.plot
#' @export
#' @return NULL, called for side effects.
#' @examples
#' plot(monthly(CA003076680), "tmax")
plot.ghcn_monthly <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_monthly"))
  
  args <- list(...)
  x[["date"]] <- as.Date(paste(x[["year"]], x[["month"]], "01", sep = "-"))
  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Viridis")
  }
  names(palette) <- unique(x[["station"]])

  if (missing(variable)) {
    variables <- intersect(c("tmax", "tmin", "prcp"), colnames(x))
    stopifnot(length(variables) > 0)
    for (v in variables) {
      interaction.plot(
        x.factor = x[["date"]],
        trace.factor = x[["station"]],
        response = x[[v]],
        trace.label = "Station",
        lty = 1,
        col = palette,
        xlab = "",
        ylab = toupper(v),
        ...
      )
      if (interactive() && variable != variables[length(variables)]) {
        readline("Press any key to show the next variable")
      }
    }
  } else {
    stopifnot(variable %in% colnames(x))
    interaction.plot(
      x.factor = x[["date"]],
      trace.factor = x[["station"]],
      response = x[[variable]],
      trace.label = "Station",
      lty = 1,
      col = palette,
      xlab = "",
      ylab = toupper(variable),
      ...
    )
  }
}

#' @title Plot GHCN Timeseries
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @param variable Name of the variable to plot.
#' @param ... additional arguments to be passed to [stats::interaction.plot()].
#' @importFrom grDevices hcl.colors
#' @importFrom graphics plot axis.Date axis lines par
#' @export
#' @return NULL, called for side effects.
#' @examples
#' plot(annual(CA003076680), "tmax")
plot.ghcn_annual <- function(x, variable, ...) {
  stopifnot(inherits(x, "ghcn_annual"))
  
  args <- list(...)
  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Viridis")
  }
  names(palette) <- unique(x[["station"]])

  if (missing(variable)) {
    variables <- intersect(c("tmax", "tmin", "prcp"), colnames(x))
    stopifnot(length(variables) > 0)    
    for (v in variables) {
      interaction.plot(
        x.factor = x[["year"]],
        trace.factor = x[["station"]],
        response = x[[v]],
        trace.label = "Station",
        lty = 1,
        col = palette,
        xlab = "",
        ylab = toupper(v),
        ...
      )
      if (interactive() && v != variables[length(variables)]) {
        readline("Press any key to show the next variable")
      }
    }
  } else {
    stopifnot(variable %in% colnames(x))
    interaction.plot(
      x.factor = x[["year"]],
      trace.factor = x[["station"]],
      response = x[[variable]],
      trace.label = "Station",
      lty = 1,
      col = palette,
      xlab = "",
      ylab = toupper(variable),
      ...
    )
  }
}

#' @title Plot GHCN Timeseries
#' @param x Object of class `ghcn_daily`. See [daily()] for details.
#' @param cutoff Numeric, the year used as cutoff for baseline.
#' @param ... additional arguments to be passed to [stats::interaction.plot()].
#' @importFrom grDevices hcl.colors
#' @importFrom stats interaction.plot loess
#' @importFrom graphics abline
#' @export
#' @return NULL, called for side effects.
#' @examples
#' plot(CA003076680, "tmax")
plot.ghcn_anomaly <- function(x, cutoff, ...) {
  stopifnot(inherits(x, "ghcn_anomaly"))

  variables <- intersect(c("tmax", "tmin"), colnames(x))
  stopifnot(length(variables) > 0)
  if (length(variables) == 2) {
    op <- par(no.readonly = TRUE)
    par(mfrow = c(2, 1))
  }

  if ("tmax" %in% variables) {
    plot(
      x[["year"]], x[["tmax"]],
      pch = 21, bg = "tomato", cex = 2,
      xlab = "", ylab = "Anomaly TMAX"
    )
    l <- loess(tmax ~ year, data = x, ...)
    lines(
      sort(l[["x"]]), l[["fitted"]][order(l[["x"]])],
      col = "tomato", lw = 2
    )
    abline(h = 0, lty = 2)
    if (!missing(cutoff)) abline(v = cutoff, lty = 2)
  }

  if ("tmin" %in% variables) {
    plot(
      x[["year"]], x[["tmin"]],
      pch = 21, bg = "dodgerblue", cex = 2,
      xlab = "", ylab = "Anomaly TMIN"
    )
    l <- loess(tmin ~ year, data = x, ...)
    lines(
      sort(l[["x"]]), l[["fitted"]][order(l[["x"]])],
      col = "dodgerblue", lw = 2
    )
    abline(h = 0, lty = 2)
    if (!missing(cutoff)) abline(v = cutoff, lty = 2)
  }

  if (length(variables) == 2) par(mfrow = c(2, 1))

}