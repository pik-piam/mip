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
#' @param data A quitte object or an object that can be transformed into a
#'   quitte object.
#' @param vars A character vector. The variables to be plotted.
#' @param tot A single string. A total value to be shown in the area plots.
#' @param fill Logical. Should the vars be normalized so that their values add
#'   to 1? (Plot shares instead of absolute values.)
#' @param orderVars In what order should the variables be shown?
#'   \describe{
#'     \item{\code{"mean"}}{
#'       Order by mean value
#'       (largest mean at bottom).
#'       The default.}
#'     \item{\code{"user"}}{
#'       As supplied by the user
#'       (first element of \code{vars} at top).}
#'     \item{\code{"userRev"}}{
#'       Reverse order of the one supplied by the user
#'       (first element of \code{vars} at bottom).}
#'   }
#' @param mainReg A single string. The plots for this region are shown enlarged.
#'   Use \code{options(mip.mainReg=<value>)} to set globally.
#' @param yearsBarPlot A numeric vector. The years shown in the bar plots. Use
#'   \code{options(mip.yearsBarPlot=<value>)} to set globally.
#' @param scales adjusts how axes are harmonized. Default is free_y
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{page 1 - \code{fill=FALSE}: \figure{showAreaAndBarPlots1.png}{options: width="100\%"}}
#' \if{html}{page 2 - \code{fill=FALSE}: \figure{showAreaAndBarPlots2.png}{options: width="100\%"}}
#' \if{html}{page 1 - \code{fill=TRUE}: \figure{showAreaAndBarPlots_fill1.png}{options: width="100\%"} }
#' \if{html}{page 2 - \code{fill=TRUE}: \figure{showAreaAndBarPlots_fill2.png}{options: width="100\%"} }
#' @examples
#' \dontrun{
#' options(mip.yearsBarPlot = c(2010, 2030, 2050, 2100))
#' options(mip.mainReg = "World")
#' data <- as.quitte(data)
#' vars <- c(
#'   "FE|CDR",
#'   "FE|Transport",
#'   "FE|Buildings",
#'   "FE|Industry")
#' showAreaAndBarPlots(data, vars)
#' showAreaAndBarPlots(data, vars, orderVars = "user")
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom dplyr rename left_join summarize group_by arrange filter
showAreaAndBarPlots <- function(
  data, vars, tot = NULL, fill = FALSE,
  orderVars = c("mean", "user", "userRev"),
  mainReg = getOption("mip.mainReg"),
  yearsBarPlot = getOption("mip.yearsBarPlot"),
  scales = "free_y"
) {

  data <- as.quitte(data)
  orderVars <- match.arg(orderVars)

  # Validate function arguments.
  stopifnot(is.character(vars))
  stopifnot((is.character(tot) && length(tot) == 1) || is.null(tot))
  stopifnot(is.logical(fill) && length(fill) == 1)
  checkGlobalOptionsProvided(c("mainReg", "yearsBarPlot"))
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  stopifnot(is.numeric(yearsBarPlot))

  if (fill) {
    if (is.null(tot))
      stop("fill=TRUE without explicit tot variable is not implemented yet")
    data <- data %>%
      calculateRatio(vars, tot)
    # `data` does not contain `tot` anymore, but its normalized values are all 1 anyway
    tot <- NULL
  }

  dnohist <- data %>%
    filter(.data$variable %in% c(.env$vars, tot), .data$scenario != "historical") %>%
    droplevels()
  if (!"identifier" %in% names(dnohist)) dnohist$identifier <- identifierModelScen(dnohist)
  d <- dnohist %>%
    filter(.data$variable %in% .env$vars, .data$scenario != "historical") %>%
    droplevels()
  warnMissingVars(d, vars)
  if (!is.null(tot)) warnMissingVars(data, tot)
  if (NROW(d) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  if (!mainReg %in% unique(d$region)) {
    warning("Main region not found in data. Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  switch(
    orderVars,
    mean = {
      # Order variables by mean value.
      # d <- mutate(d, variable = forcats::fct_reorder(.data$variable, .data$value, mean, na.rm = TRUE))
      # To not use the additional package forcats, implement own version of that line:
      means <- d %>%
        group_by(.data$variable) %>%
        summarize(mean_value = mean(.data$value, na.rm = TRUE)) %>%
        arrange(.data$mean_value)
      d$variable <- factor(d$variable, levels = means$variable)
    },
    user = {
      d$variable <- factor(d$variable, levels = vars)
    },
    userRev = {
      d$variable <- factor(d$variable, levels = rev(vars))
    }
  )

  # Common label for y-axis.
  lcp <- if (is.null(tot)) gsub("\\|$", "", attr(shorten_legend(vars, identical_only = TRUE), "front")) else gsub(" pCap$", "", tot)
  label <- paste0(lcp, " (", paste0(levels(d$unit), collapse = ","), ")")

  # Create plots.
  p1 <- d %>%
    filter(.data$region == .env$mainReg) %>%
    droplevels() %>%
    mipArea(scales = scales, total = is.null(tot), ylab = lcp) +
    ylab(NULL) +
    theme(legend.position = "none")
  p2 <- d %>%
    filter(.data$region == .env$mainReg, .data$period %in% .env$yearsBarPlot) %>%
    droplevels() %>%
    mipBarYearData(ylab = lcp) +
    ylab(NULL)

  d3 <- d %>%
    filter(.data$region != .env$mainReg,
           .data$period %in% .env$yearsBarPlot)
  d4 <- d %>%
    filter(.data$region != .env$mainReg)
  showNonMainRegs <- nrow(d3) > 0 && nrow(d4) > 0

  if (showNonMainRegs) {
    p2 <- p2 +
      theme(legend.position = "none")
    p3 <- d3 %>%
      droplevels() %>%
      mipBarYearData(ylab = lcp) +
      ylab(NULL) +
      guides(fill = guide_legend(reverse = TRUE, ncol = 3))
    p4 <- d4 %>%
      droplevels() %>%
      mipArea(scales = scales, total = is.null(tot), ylab = lcp,
              stack_priority = if (is.null(tot)) c("variable", "region") else "variable") +
      guides(fill = guide_legend(reverse = TRUE))
  } else {
    p2 <- p2 +
      guides(fill = guide_legend(reverse = TRUE, ncol = 3))
  }

  # Add black lines in area plots from variable tot if provided.
  if (!is.null(tot)) {
    dMainTot <- dnohist %>%
      filter(
        .data$region == .env$mainReg,
        .data$variable == .env$tot) %>%
      droplevels()
    p1 <- p1 +
      geom_line(
        data = dMainTot,
        mapping = aes(.data$period, .data$value),
        size = 1.3
      )
    if (showNonMainRegs) {
      dRegiTot <- dnohist %>%
        filter(
          .data$region != .env$mainReg,
          .data$variable == .env$tot) %>%
        droplevels()
      dRegiTot$scenario <- dRegiTot$identifier
      p4 <- p4 +
        geom_line(
          data = dRegiTot,
          mapping = aes(.data$period, .data$value),
          size = 1.3
        )
    }
  }

  cat("\\clearpage")

  # Show plots.
  if (showNonMainRegs) {
    grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 3), c(2, 3)), left = label)
    cat("\n\n")
    print(p4)
  } else {
    grid.arrange(p1, p2, layout_matrix = rbind(c(1, 2)), left = label)
  }
  cat("\n\n")

  return(invisible(NULL))
}
