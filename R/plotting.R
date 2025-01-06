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
#' @param ... additional arguments to be passed to [stats::interaction.plot()].
#' @importFrom grDevices hcl.colors
#' @importFrom stats interaction.plot loess
#' @importFrom graphics abline
#' @export
#' @return NULL, called for side effects.
#' @examples
#' plot(anomaly(remove_flagged(CA003076680), 2015))
plot.ghcn_anomaly <- function(x, ...) {
  stopifnot(inherits(x, "ghcn_anomaly"))
  if (length(unique(x[["station"]])) > 1) {
    stop("Cannot plot anomaly for multiple stations when not aggregated. See ?anomaly() for details.")
  }

  variables <- intersect(c("tmax", "tmin"), colnames(x))
  stopifnot(length(variables) > 0)
  if (length(variables) == 2) {
    op <- par(no.readonly = TRUE)
    par(mfrow = c(2, 1))
  }

  if ("tmax" %in% variables) {
    barplot(
      x[["tmax"]],
      col = ifelse(x[["tmax"]] <= 0, "dodgerblue", "tomato"),
      xlab = "", ylab = "Anomaly TMAX",
      names.arg = x[["year"]]
    )
  }

  if ("tmin" %in% variables) {
    barplot(
      x[["tmin"]],
      col = ifelse(x[["tmin"]] <= 0, "dodgerblue", "tomato"),
      xlab = "", ylab = "Anomaly TMIN",
      names.arg = x[["year"]]
    )
  }

  if (length(variables) == 2) par(op)

}
