#' Show Multi-Line Plots by Variable
#'
#' Show plots with different regions in the same plot; x-axis variable chosen by
#' user.
#'
#' Same as \code{\link{showMultiLinePlots}} but with the variable specified by
#' \code{xVar} on x-axis. For every y-axis-value, we need a unique x-axis-value.
#' For historical data, there may be several sources / models of the same
#' variable. For the x-axis-variable a unique historical source / model is
#' chosen via \code{histRefModel}.
#'
#' @param xVar A single string. The variable for the x-axis.
#' @param showHistorical A single logical value. Should historical data be
#'   shown? It is not recommended to set this to \code{TRUE} as the resulting
#'   plot we probably be quite confusing.
#' @param histRefModel A named character vector identifying the unique model to
#'   be chosen for historical data. Use \code{options(mip.histRefModel=<value>)}
#'   to set globally.
#' @param yearsByVariable A numeric vector. The years to be marked in the plots.
#'   As default it uses the value globally set by \code{options(mip.yearsBarPlot=<value>)}.
#' @inheritParams showMultiLinePlots
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{page 1: \figure{showMultiLinePlotsByVariable1.png}{options: width="100\%"}}
#' \if{html}{page 2: \figure{showMultiLinePlotsByVariable2.png}{options: width="100\%"}}
#' @examples
#' \dontrun{
#' options(mip.mainReg = "World")
#' options(mip.yearsBarPlot = c(2010, 2030, 2050, 2100))
#' options(mip.histRefModel = c("GDP|PPP pCap" = "James_IMF"))
#' data <- as.quitte(data)
#' vars <- c(
#'   "FE|Transport pCap",
#'   "FE|Buildings pCap",
#'   "FE|Industry pCap")
#' showMultiLinePlotsByVariable(data, vars, "GDP|PPP pCap")
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 ylim
showMultiLinePlotsByVariable <- function(
  data, vars, xVar, scales = "free_y",
  showHistorical = FALSE,
  mainReg = getOption("mip.mainReg"),
  histRefModel = getOption("mip.histRefModel"),
  yearsByVariable = getOption("mip.yearsBarPlot")
) {

  data <- as.quitte(data)

  # Validate function arguments.
  stopifnot(is.character(vars))
  stopifnot(is.character(xVar) && length(xVar) == 1)
  stopifnot(is.character(scales) && length(scales) == 1)
  stopifnot(identical(showHistorical, TRUE) || identical(showHistorical, FALSE))
  stopifnot(is.null(yearsByVariable) || is.numeric(yearsByVariable))
  checkGlobalOptionsProvided(c("mainReg", "histRefModel"))
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  stopifnot(is.character(histRefModel) && !is.null(names(histRefModel)))
  stopifnot(xVar %in% names(histRefModel))

  dy <- data %>%
    filter(.data$variable %in% .env$vars)
  dx <- data %>%
    filter(.data$variable %in% .env$xVar) %>%
    filter(.data$scenario != "historical" | .data$model == .env$histRefModel[.env$xVar])
  d <- dy %>%
    left_join(dx, by = c("scenario", "region", "period"), suffix = c("", ".x")) %>%
    drop_na(.data$value, .data$value.x) %>%
    arrange(.data$period) %>%
    droplevels()
  dMainScen <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario != "historical") %>%
    droplevels()
  dMainHist <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario == "historical") %>%
    droplevels()
  dRegiScen <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario != "historical") %>%
    droplevels()
  dRegiHist <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario == "historical") %>%
    droplevels()

  regions <- levels(dRegiScen$region)

  warnMissingVars(dMainScen, vars)
  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  label <- paste0("(", paste0(levels(d$unit), collapse = ","), ")")
  xLabel <- paste0(xVar, " (", paste0(levels(d$unit.x), collapse = ","), ")")

  p1 <- dMainScen %>%
    ggplot(aes(.data$value.x, .data$value)) +
    geom_line(aes(linetype = .data$scenario)) +
    facet_wrap(vars(.data$variable), scales = scales) +
    theme_minimal() +
    expand_limits(y = 0) +
    ylab(label) + xlab(xLabel)
  p2 <- dRegiScen %>%
    ggplot(aes(.data$value.x, .data$value, color = .data$region)) +
    geom_line(aes(linetype = .data$scenario)) +
    facet_wrap(vars(.data$variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    expand_limits(y = 0) +
    ylab(label) + xlab(xLabel)
  if (showHistorical) {
    p1 <- p1 +
      geom_point(data = dMainHist, aes(shape = .data$model)) +
      geom_line(data = dMainHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5)
    p2 <- p2 +
      geom_point(data = dRegiHist, aes(shape = .data$model)) +
      geom_line(data = dRegiHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5)
  }
  # Add markers for certain years.
  if (length(yearsByVariable) > 0) {
    p1 <- p1 +
      geom_point(
      data = dMainScen %>%
        filter(.data$period %in% .env$yearsByVariable) %>%
        mutate(year = factor(.data$period)),
      mapping = aes(.data$value.x, .data$value, shape = .data$year))
    p2 <- p2 +
      geom_point(
        data = dRegiScen %>%
          filter(.data$period %in% .env$yearsByVariable) %>%
          mutate(year = factor(.data$period)),
        mapping = aes(.data$value.x, .data$value, shape = .data$year))
  }

  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")

  return(invisible(NULL))
}
