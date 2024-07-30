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
#' @param scales A single string. choose either \code{"free_y"} or \code{"fixed"}.
#' @param color.dim.name name for the color-dimension used in the legend
#' @param histModels A character vector filtering the historical models to include.
#' Set to \code{NULL} (default) for no filtering.
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
showLinePlots <- function(
    data, vars = NULL, scales = "free_y", color.dim.name = NULL,
    mainReg = getOption("mip.mainReg"),
    histModels = NULL
) {

  data <- as.quitte(data) %>%
    filter(!is.na(.data$value))

  # Validate function arguments.
  stopifnot(is.character(vars) || is.null(vars))
  stopifnot(is.character(histModels) || is.null(histModels))
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)

  if (!is.null(vars)) {
    d <- data %>%
      filter(.data$variable %in% .env$vars) %>%
      droplevels()
    unitlabel <- ifelse(length(levels(d$unit)) == 0, "", paste0(" (", paste0(levels(d$unit), collapse = ","), ")"))
    label <- paste0(paste0(vars, collapse = ","), unitlabel)
  } else {
    d <- data %>%
      droplevels()
    unitlabel <- ifelse(length(levels(d$unit)) == 0, "", paste0(" (", paste0(levels(d$unit), collapse = ","), ")"))
    label <- paste0(paste0(levels(d$variable), collapse = ","), unitlabel)
  }

  if (!is.null(histModels)) {
    d <- d %>%
      filter(.data$scenario != "historical" | .data$model %in% .env$histModels)
  }

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
        color.dim.manual.hist = color.dim.manual.hist[mainHistModels]
      )
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
        color.dim.manual.hist = color.dim.manual.hist[regiHistModels]
      )
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
