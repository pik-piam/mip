#' Show Line Plots
#'
#' Creates 2 sets of line plots from scenario data and shows them.
#'
#' Two sets of line plots are shown (main region + others), depicting the values
#' in \code{vars} over time. Faceting is done by \code{region}. The plots
#' arranged and shown.
#'
#' @param vars A character vector. Usually just a single string. The variables
#'   to be plotted. If \code{NULL} all rows from \code{data} are plotted.
#' @param histVars A character vector. Usually just a single string. The historical variables
#'   to be plotted. If \code{NULL}, it is set to \code{vars}.
#' @param scales A single string. choose either \code{"free_y"} or \code{"fixed"}.
#' @param color.dim.name name for the color-dimension used in the legend
#' @param histModelsExclude A character vector with historical models to exclude.
#' Set to \code{NULL} (default) for all available data.
#' @param color.dim.manual optional vector with manual colors replacing default
#' colors of color.dim, default is \code{NULL}.
#' @param vlines period used for vertical line
#' @inheritParams showAreaAndBarPlots
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{\figure{showLinePlots.png}{options: width="100\%"}}
#' @examples
#' \dontrun{
#' options(mip.mainReg = "World")
#' data <- as.quitte(data)
#' showLinePlots(data, "Policy Cost|GDP Loss")
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom dplyr bind_rows
#' @importFrom gridExtra arrangeGrob

showLinePlots <- function(
    data,
    vars = NULL,
    histVars = NULL,
    scales = "free_y",
    color.dim.name = NULL,
    mainReg = getOption("mip.mainReg"),
    color.dim.manual = NULL,
    histModelsExclude = NULL,
    vlines = NULL
) {

  data <- as.quitte(data) %>%
    filter(!is.na(.data$value))

  # Validate function arguments.
  stopifnot(is.character(vars) || is.null(vars))
  stopifnot(is.character(histVars) || is.null(histVars))
  stopifnot(is.character(histModelsExclude) || is.null(histModelsExclude))
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)

  if (is.null(histVars)) {
    histVars <- vars
  }

  if (!is.null(vars)) {
    v <- unique(c(vars, histVars))
    d <- data %>%
      filter(.data$variable %in% .env$v) %>%
      droplevels()
    unitlabel <- ifelse(length(levels(d$unit)) == 0, "", paste0(" (", paste0(levels(d$unit), collapse = ","), ")"))
    label <- paste0(paste0(v, collapse = ","), unitlabel)
  } else {
    d <- data %>%
      droplevels()
    unitlabel <- ifelse(length(levels(d$unit)) == 0, "", paste0(" (", paste0(levels(d$unit), collapse = ","), ")"))
    label <- paste0(paste0(levels(d$variable), collapse = ","), unitlabel)
  }

  if (!is.null(histModelsExclude)) {
    d <- d %>%
      filter(.data$scenario != "historical" | !.data$model %in% .env$histModelsExclude)
  }

  dMainScen <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario != "historical", .data$variable %in% .env$vars) %>%
    droplevels()
  dMainHist <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario == "historical", .data$variable %in% .env$histVars) %>%
    droplevels()
  dRegiScen <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario != "historical", .data$variable %in% .env$vars) %>%
    droplevels()
  dRegiHist <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario == "historical", .data$variable %in% .env$histVars) %>%
    droplevels()

  # make sure all plots use the same colors for historical models
  mainHistModels <- levels(dMainHist$model)
  regiHistModels <- levels(dRegiHist$model)

  if (identical(mainHistModels, regiHistModels)) {
    color.dim.manual.hist <- NULL
  } else {
    color.dim.manual.hist <- plotstyle(union(mainHistModels, regiHistModels))
  }

  if (!is.null(vars))
    warnMissingVars(bind_rows(dMainScen, dRegiScen), vars)
  if (NROW(dMainScen) == 0 && NROW(dRegiScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  if (NROW(dMainScen) == 0) {
    p1 <- ggplot() + theme_minimal()
  } else {
    p1 <- dMainScen %>%
      mipLineHistorical(
        x_hist = dMainHist,
        ylab = label,
        scales = scales,
        plot.priority = c("x_hist", "x", "x_proj"),
        color.dim.name = color.dim.name,
        color.dim.manual = color.dim.manual,
        color.dim.manual.hist = color.dim.manual.hist[mainHistModels]
      )
    if (! is.null(vlines)) {
      p1 <- p1 + geom_vline(xintercept = vlines, linetype = 3)
    }
  }
  if (NROW(dRegiScen) == 0) {
    p2 <- ggplot() + theme_minimal()
  } else {
    p2 <- dRegiScen %>%
      mipLineHistorical(
        x_hist = dRegiHist,
        ylab = NULL,
        scales = scales,
        plot.priority = c("x_hist", "x", "x_proj"),
        facet.ncol = 3,
        color.dim.name = color.dim.name,
        color.dim.manual = color.dim.manual,
        color.dim.manual.hist = color.dim.manual.hist[regiHistModels]
      )
    if (! is.null(vlines)) {
      p2 <- p2 + geom_vline(xintercept = vlines, linetype = 3)
    }
  }

  # If a legend of the plots can be used as common legend for both plots,
  # show that legend below mainReg-plot and only that legend.

  if (length(setdiff(regiHistModels, mainHistModels)) == 0) {
    lgnd <- getLegend(p1)
  } else if (length(setdiff(mainHistModels, regiHistModels)) == 0) {
    lgnd <- getLegend(p2)
  } else {
    lgnd <- NULL
  }

  if (!is.null(lgnd)) {
    p1 <- arrangeGrob(
      p1 + theme(legend.position = "none"),
      lgnd,
      ncol = 1,
      heights = c(0.76, 0.24)
    )
    p2 <- p2 + theme(legend.position = "none")
  }

  # Show plots.
  grid.arrange(p1, p2, nrow = 1)
  cat("\n\n")

  return(invisible(NULL))
}
