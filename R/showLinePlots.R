#' Show Line Plots
#'
#' Creates 2 sets of line plots from scenario data and shows them.
#'
#' Two sets of line plots are shown (main region + others), depicting the values
#' in \code{vars} over time. Faceting is done by \code{region}. The plots
#' arranged and shown.
#'
#' @md
#' @inheritDotParams createLinePlots
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
showLinePlots <- function(...) {
  showPlot(createLinePlots(...))
  cat("\n\n")
  return(invisible(NULL))
}

#' Create Line Plots
#'
#' Creates the line plots for showLinePlots.
#' @param vars A character vector of variables to be plotted. Defaults to all
#'   variables in `data`.
#' @param histVars A character vector of historical variables to be plotted.
#'   Defaults to `vars`.
#' @param scales A single string. choose either \code{"free_y"} or \code{"fixed"}.
#' @param ylim   y limits
#' @param show.dots   If TRUE: shows geom_point dots on line. If FALSE: only plots geom_line without geom_point
#' @param color.dim.name name for the color-dimension used in the legend
#' @param histModelsExclude A character vector with historical models to
#'   exclude.
#' @param color.dim.manual optional vector with manual colors replacing default
#' colors of color.dim, default is \code{NULL}.
#' @param target optional string, model variable to be plotted with dots (indicating targets)
#' @param vlines period used for vertical line
#' @return Arranged plots
#' @inheritParams createAreaAndBarPlots
#' @importFrom dplyr bind_rows
#' @importFrom gridExtra arrangeGrob
#' @importFrom quitte as.quitte getVars
#' @importFrom rlang .data .env
createLinePlots <- function(
  data,
  vars = getVars(as.quitte(data)),
  histVars = vars,
  scales = "free_y",
  ylim = 0,
  show.dots = TRUE,
  color.dim.name = NULL,
  mainReg = getOption("mip.mainReg"),
  color.dim.manual = NULL,
  histModelsExclude = character(),
  target = NULL,
  vlines = NULL
) {
  # Validate function arguments.
  stopifnot(is.character(vars))
  stopifnot(is.character(histVars))
  stopifnot(is.character(histModelsExclude))
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)

  d <- as.quitte(data) %>%
    filter(!is.na(.data$value),
           ((.data$variable %in% .env$vars & .data$scenario != "historical")           |
 (.data$variable %in% .env$histVars             &
  .data$scenario ==   "historical"             &
 !.data$model    %in% .env$histModelsExclude)
           )) %>%
    droplevels()

  unitlabel <- ifelse(identical("(Missing)", levels(d$unit)), "",
                      paste0(" (", paste0(levels(d$unit), collapse = ","), ")"))
  label <- paste0(paste0(unique(d$variable), collapse = ","), unitlabel)

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

  warnMissingVars(bind_rows(dMainScen, dRegiScen), vars)

  if (NROW(dMainScen) == 0 && NROW(dRegiScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(NULL)
  }

  if (NROW(dMainScen) == 0) {
    p1 <- ggplot() + theme_minimal()
  } else {
    p1 <- dMainScen %>%
      mipLineHistorical(
        x_hist = dMainHist,
        ylab = label,
        scales = scales,
        ylim = ylim,
        show.dots = show.dots,
        plot.priority = c("x_hist", "x", "x_proj"),
        color.dim.name = color.dim.name,
        color.dim.manual = color.dim.manual,
        color.dim.manual.hist = color.dim.manual.hist[mainHistModels]
      )

    # make World match regional facets for fixed scales
    if ('fixed' == scales) {
      p1 <- p1 +
        geom_blank(
          data = dRegiScen %>%
            select(-'region'),
          mapping = aes(x = .data$period, y = .data$value))
    }

    if (!is.null(vlines)) {
      p1 <- p1 + geom_vline(xintercept = vlines, linetype = 3)
    }

    if (!is.null(target)) {

      targets <- as.quitte(data) %>%
        filter(!is.na(.data$value), .data$region == .env$mainReg,
               .data$scenario != "historical", .data$variable == target) %>%
        droplevels()

      p1 <- p1 +
        geom_point(data = targets,
                   aes(x = .data$period, y = .data$value, shape = .data$scenario)) +
        guides(shape = guide_legend(title = "Target",
                                    theme = theme(legend.direction = "vertical",)))
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
        ylim = ylim,
        show.dots = show.dots,
        plot.priority = c("x_hist", "x", "x_proj"),
        facet.ncol = 3,
        color.dim.name = color.dim.name,
        color.dim.manual = color.dim.manual,
        color.dim.manual.hist = color.dim.manual.hist[regiHistModels]
      )

    if (!is.null(vlines)) {
      p2 <- p2 + geom_vline(xintercept = vlines, linetype = 3)
    }

    if (!is.null(target)) {

      targets <- as.quitte(data) %>%
        filter(!is.na(.data$value), .data$region != .env$mainReg,
               .data$scenario != "historical", .data$variable == target) %>%
        droplevels()

      p2 <- p2 +
        geom_point(data = targets,
                   aes(x = .data$period, y = .data$value, shape = .data$scenario)) +
        guides(shape = guide_legend(title = "Target",
                                    theme = theme(legend.direction = "vertical")))

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
      heights = c(0.6, 0.4)
    )
    p2 <- p2 + theme(legend.position = "none")
  }

  return(arrangeGrob(p1, p2, nrow = 1, widths = c(2, 3)))
}
