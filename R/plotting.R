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
  if (missing(variable)) {
    variables <- intersect(c("tmax", "tmin", "prcp"), colnames(x))
  }
  stopifnot(variable %in% colnames(x))
  
  args <- list(...)

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Viridis")
  }
  names(palette) <- unique(x[["station"]])
  
  for (variable in variables) {
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
    if (interactive() && variable != variables[length(variables)]) {
      readline("Press any key to show the next variable")
    }
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
  if (missing(variable)) {
    variables <- intersect(c("tmax", "tmin", "prcp"), colnames(x))
  }
  stopifnot(variable %in% colnames(x))
  
  args <- list(...)
  
  x[["date"]] <- as.Date(paste(x[["year"]], x[["month"]], "01", sep = "-"))

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Viridis")
  }
  names(palette) <- unique(x[["station"]])
  
  for (variable in variables) {
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
    if (interactive() && variable != variables[length(variables)]) {
      readline("Press any key to show the next variable")
    }
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
  if (missing(variable)) {
    variables <- intersect(c("tmax", "tmin", "prcp"), colnames(x))
  }
  stopifnot(variable %in% colnames(x))
  
  args <- list(...)

  n <- length(unique(x[["station"]]))
  if (n == 1) {
    palette <- "grey20"
  } else {
    palette <- hcl.colors(n, "Viridis")
  }
  names(palette) <- unique(x[["station"]])
  
  for (variable in variables) {
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
    if (interactive() && variable != variables[length(variables)]) {
      readline("Press any key to show the next variable")
    }
  }
}
