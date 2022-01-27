#' Show Multi-Line Plots
#'
#' Show 2 sets of plots with different regions in the same plot (value over time).
#'
#' Creates two plots (main region + others) with the values of \code{vars} over
#' time. Different regions are shown in the same plot. Faceting is done by
#' \code{variable}. The plots arranged and shown.
#'
#' @param vars A character vector. The variables to be plotted.
#' @inheritParams showLinePlots
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{page 1: \figure{showMultiLinePlots1.png}{options: width="100\%"}}
#' \if{html}{page 2: \figure{showMultiLinePlots2.png}{options: width="100\%"}}
#' @examples
#' \dontrun{
#' options(mip.mainReg = "World")
#' data <- as.quitte(data)
#' vars <- c(
#'   "FE|Transport pCap",
#'   "FE|Buildings pCap",
#'   "FE|Industry pCap")
#' showMultiLinePlots(data, vars)
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom ggplot2 ylim
showMultiLinePlots <- function(
  data, vars, scales = "free_y",
  mainReg = getOption("mip.mainReg")
) {
  
  data <- as.quitte(data)

  # Validate function arguments.
  stopifnot(is.character(vars))
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)

  data %>%
    filter(.data$variable %in% .env$vars) %>%
    droplevels() ->
    d
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
  regions <- levels(dRegiScen$region)

  warnMissingVars(dMainScen, vars)
  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  label <- paste0("[", paste0(levels(d$unit), collapse = ","), "]")

  dMainScen %>%
    ggplot(aes(.data$period, .data$value)) +
    geom_line(aes(linetype = .data$scenario)) +
    geom_point(data = dMainHist, aes(shape = .data$model)) +
    geom_line(data = dMainHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5) +
    facet_wrap(vars(.data$variable), scales = scales) +
    theme_minimal() +
    ylim(0, NA) +
    ylab(label) ->
    p1
  dRegiScen %>%
    ggplot(aes(.data$period, .data$value, color = .data$region)) +
    geom_line(aes(linetype = .data$scenario)) +
    geom_point(data = dRegiHist, aes(shape = .data$model)) +
    geom_line(data = dRegiHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5) +
    facet_wrap(vars(.data$variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    ylim(0, NA) +
    ylab(label) ->
    p2

  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")

  return(invisible(NULL))
}
