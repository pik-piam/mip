#' Show Area and Bar Plots
#'
#' Creates 4 sets of plots from scenario data and shows them.
#'
#' Creates 2 sets of area plots (main region + others) and 2 sets of bar plots
#' (main region + others) of the variables specified in vars over time. For area
#' plots, faceting is done by region and scenario; for bar plots over region. If
#' a variables is given in tot, this is shown as a black line. If not the sum of
#' the values of vars is drawn. If \code{fill=TRUE}, the values of vars are
#' divided by the values of tot to show share of total. The plots arranged and
#' shown.
#' 
#' @param data A quitte object.
#' @param vars A character vector. The variables to be plotted.
#' @param tot A single string. A total value to be shown in the area plots.
#' @param fill Logical. Should the vars be normalized so that their values add to 1? (Plot shares instead of absolute values.)
#' @param mainReg A single string. The plots for this region are shown enlarged. Use \code{options(mip.mainReg=<value>)} to set globally.
#' @param yearsBarPlot A numeric vector. The years shown in the bar plots. Use \code{options(mip.yearsBarPlot=<value>)} to set globally.
#' @return \code{NULL} is returned invisible.
#' @examples
#' \dontrun{
#' items <- c(
#'   "FE|CDR",
#'   "FE|Transport",
#'   "FE|Buildings",
#'   "FE|Industry")
#' showAreaAndBarPlots(data, items)
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom dplyr rename left_join
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_remove
showAreaAndBarPlots <- function(
  data, vars, tot = NULL, fill = FALSE,
  mainReg = getOption("mip.mainReg"),
  yearsBarPlot = getOption("mip.yearsBarPlot")
) {
  
  # Validate function arguments.
  stopifnot(is.quitte(data))
  stopifnot(is.character(vars))
  stopifnot((is.character(tot) && length(tot) == 1) || is.null(tot))
  stopifnot(is.logical(fill) && length(fill) == 1)
  checkGlobalOptionsProvided(c("mainReg", "yearsBarPlot"))
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  stopifnot(is.numeric(yearsBarPlot))
  
  if (fill) {
    if (is.null(tot))
      stop("fill=TRUE without explicit tot variable is not implemented yet")
    data %>%
      calacuateRatio(vars, tot) ->
      data
  }
  
  data %>%
    filter(.data$variable %in% .env$vars, .data$scenario != "historical") ->
    d
  warnMissingVars(d, vars)
  if (!is.null(tot)) warnMissingVars(data, tot)
  if (NROW(d) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  # Order variables by mean value.
  d %>%
    mutate(variable = fct_reorder(.data$variable, .data$value, mean, na.rm=TRUE)) %>% 
    droplevels() ->
    d
  
  # Common label for y-axis.
  lcp <- str_remove(longestCommonPrefix(vars), "\\|$")
  label <- paste0(lcp, " [", paste0(levels(d$unit), collapse = ","), "]")
  
  # Create plots.
  
  d %>%
    filter(.data$region == .env$mainReg) %>%
    droplevels() %>% 
    mipArea(scales = "free_y", total = is.null(tot)) +
    ylab(NULL) +
    theme(legend.position = "none") ->
    p1
  
  d %>%
    filter(.data$region == .env$mainReg, .data$period %in% .env$yearsBarPlot) %>%
    droplevels() %>% 
    mipBarYearData() +
    ylab(NULL) +
    theme(legend.position = "none") ->
    p2
  
  d %>%
    filter(.data$region != .env$mainReg, .data$period %in% .env$yearsBarPlot) %>%
    droplevels() %>% 
    mipBarYearData() +
    ylab(NULL) +
    guides(fill = guide_legend(reverse = TRUE, ncol = 3)) ->
    p3
  
  d %>%
    filter(.data$region != .env$mainReg) %>%
    droplevels() %>% 
    mipArea(scales = "free_y", total = is.null(tot)) +
    guides(fill = guide_legend(reverse = TRUE)) ->
    p4
  
  # Add black lines in area plots from variable tot if provided.
  if (!is.null(tot) && !fill) {
    data %>% 
      filter(
        .data$region == .env$mainReg, 
        .data$variable == .env$tot, 
        .data$scenario != "historical") %>% 
      droplevels() ->
      dMainTot
    p1 <- p1 +
      geom_line(
        data = dMainTot,
        mapping = aes(.data$period, .data$value),
        size = 1.3
      )
    data %>% 
      filter(
        .data$region != .env$mainReg, 
        .data$variable == .env$tot, 
        .data$scenario != "historical") %>% 
      droplevels() ->
      dRegiTot
    p4 <- p4 +
      geom_line(
        data = dRegiTot,
        mapping = aes(.data$period, .data$value),
        size = 1.3
      )
  }
  
  # Show plots.
  grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 3), c(2, 3)), left = label)
  cat("\n\n")
  print(p4)
  cat("\n\n")
  
  return(invisible(NULL))
}
