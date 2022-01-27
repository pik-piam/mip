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
  data, vars = NULL, scales = "free_y",
  mainReg = getOption("mip.mainReg")
) {
  
  data <- as.quitte(data)

  # Validate function arguments.
  stopifnot(is.character(vars) || is.null(vars))
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)

  if (!is.null(vars)) {
    data %>%
      filter(.data$variable %in% .env$vars) %>%
      droplevels() ->
      d
    label <- paste0(
      paste0(vars, collapse = ","),
      " [", paste0(levels(d$unit), collapse = ","), "]")
  } else {
    data %>%
      droplevels() ->
      d
    label <- paste0(
      paste0(levels(d$variable), collapse = ","),
      " [", paste0(levels(d$unit), collapse = ","), "]")
  }

  d %>%
    filter(.data$region == .env$mainReg, .data$scenario != "historical") %>%
    droplevels() ->
    dMainScen
  d %>%
    filter(.data$region == .env$mainReg, .data$scenario == "historical") %>%
    droplevels() ->
    dMainHist
  d %>%
    filter(.data$region != .env$mainReg, .data$scenario != "historical") %>%
    droplevels() ->
    dRegiScen
  d %>%
    filter(.data$region != .env$mainReg, .data$scenario == "historical") %>%
    droplevels() ->
    dRegiHist

  if (!is.null(vars))
    warnMissingVars(bind_rows(dMainScen, dRegiScen), vars)
  if (NROW(dMainScen) == 0 && NROW(dRegiScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  if (NROW(dMainScen) == 0) {
    p1 <- ggplot() + theme_minimal()
  } else {
    dMainScen %>%
      mipLineHistorical(
        x_hist = dMainHist,
        ylab = label,
        scales = scales,
        plot.priority = c("x_hist", "x", "x_proj")
      ) ->
      p1
  }
  if (NROW(dRegiScen) == 0) {
    p2 <- ggplot() + theme_minimal()
  } else {
    dRegiScen %>%
      mipLineHistorical(
        x_hist = dRegiHist,
        ylab = NULL,
        scales = scales,
        plot.priority = c("x_hist", "x", "x_proj"),
        facet.ncol = 3
      ) ->
      p2
  }

  # If a legend of the plots can be used as common legend for both plots,
  # show that legend below mainReg-plot and only that legend.
  mainHistModels <- levels(dMainHist$model)
  regiHistModels <- levels(dRegiHist$model)
  if (length(mainHistModels) == 0 || identical(mainHistModels, regiHistModels)) {
    lgnd <- getLegend(p2)
  } else if (length(regiHistModels) == 0) {
    lgnd <- getLegend(p1)
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
