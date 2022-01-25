#' Show Line Plots for Region Comparison
#'
#' Shows line plots of a variable with different regions in the same plot and
#' faceting by scenario.
#'
#' Creates one set of plots with the values of \code{vars} over time. Does not
#' show historical data. Different regions are shown in the same plot. Faceting
#' is done by \code{scenario}. The plots arranged and shown.
#'
#' @param excludeMainRegion A single logical value. Should the main region be
#'   excluded (or shown in the same plot)?
#' @inheritParams showMultiLinePlots
#' @return \code{NULL} is returned invisible.
#' @examples
#' \dontrun{
#' showRegiLinePlots(data, "Price|Carbon")
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom ggplot2 ylim
showRegiLinePlots <- function(
  data, vars, scales = "free_y",
  excludeMainRegion = TRUE,
  mainReg = getOption("mip.mainReg")
) {

  # Validate function arguments.
  stopifnot(is.quitte(data))
  stopifnot(is.character(vars))
  stopifnot(is.character(scales) && length(scales) == 1)
  stopifnot(is.logical(excludeMainRegion) && length(excludeMainRegion) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)

  data %>%
    filter(.data$variable %in% .env$vars) %>%
    droplevels() ->
    d
  if (excludeMainRegion) {
    d %>%
      filter(.data$region != .env$mainReg) %>%
      droplevels() ->
      d
  }
  d %>%
    filter(.data$scenario != "historical") %>%
    droplevels() ->
    dScen
  regions <- levels(dScen$region)

  warnMissingVars(dScen, vars)
  if (NROW(dScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  label <- paste0(
    paste0(vars, collapse = ","),
    " [", paste0(levels(d$unit), collapse = ","), "]")

  dScen %>%
    mipLineHistorical(
      ylab = label,
      scales = scales,
      plot.priority = c("x_hist", "x", "x_proj"),
      color.dim = "region",
      facet.dim = "scenario",
      facet.ncol = 2) +
    theme(legend.position = "right") +
    scale_color_manual(values = plotstyle(regions)) +
    theme_minimal() ->
    p

  # Show plots.
  print(p)
  cat("\n\n")

  return(invisible(NULL))
}
