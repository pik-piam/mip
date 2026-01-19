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
#' @inheritDotParams layoutAreaAndBarPlots
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
showAreaAndBarPlots <- function(...) {
  plots <- layoutAreaAndBarPlots(...)
  for (plot in plots) {
    showPlot(plot)
    cat("\n\n")
  }
  return(invisible(NULL))
}



#' Layout Area and Bar Plots
#' Layouts the area and bar plots created by createAreaAndBarPlots, which can
#' then be displayed using showAreaAndBarPlots
#' @md
#' @inheritDotParams createAreaAndBarPlots
#' @return Arranged plots
#' @importFrom gridExtra arrangeGrob
#'
#' @export
layoutAreaAndBarPlots <- function(...) {

  items <- createAreaAndBarPlots(...)

  if (is.null(items) || length(items) == 0) {
    return(NULL)
  }

  if (items[["showNonMainRegs"]]) {

    plotAreaMain <- items[["p1"]][[1]]
    plotBarsMain <- items[["p1"]][[2]]
    plotBarsRegi <- items[["p1"]][[3]]
    label <- items[["p1"]][[4]]

    return(list(
      arrangeGrob(plotAreaMain, plotBarsMain, plotBarsRegi,
        layout_matrix = rbind(c(1, 3), c(2, 3)), left = label
      ),
      items[["p2"]]
    ))
  } else {

    plotAreaMain <- items[["p1"]][[1]]
    plotBarsMain <- items[["p1"]][[2]]
    label <- items[["p1"]][[3]]

    return(list(arrangeGrob(plotAreaMain, plotBarsMain,
                            layout_matrix = rbind(c(1, 2)), left = label)))

  }
}


#' Create Area and Bar Plots
#'
#' Creates the area and bar plots for layoutAreaAndBarPlots.
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
#' @return A list of plots
#' @importFrom rlang .data .env
#' @importFrom dplyr rename left_join summarize group_by arrange filter bind_rows
#' @export
createAreaAndBarPlots <- function(
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

  dataModel <- data %>%
    filter(.data$variable %in% c(.env$vars, tot), .data$scenario != "historical") %>%
    droplevels()
  if (!"identifier" %in% names(dataModel)) dataModel$identifier <- identifierModelScen(dataModel)
  dataVars <- dataModel %>%
    filter(.data$variable %in% .env$vars, .data$scenario != "historical") %>%
    droplevels()
  warnMissingVars(dataVars, vars)
  if (!is.null(tot)) warnMissingVars(data, tot)
  if (nrow(dataVars) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(list())
  }

  if (!mainReg %in% unique(dataVars$region)) {
    warning("Main region not found in data. Nothing to plot.", call. = FALSE)
    return(list())
  }

  switch(
    orderVars,
    mean = {
      # Order variables by mean value.
      means <- dataVars %>%
        group_by(.data$variable) %>%
        summarize(mean_value = mean(.data$value, na.rm = TRUE)) %>%
        arrange(.data$mean_value)
      dataVars$variable <- factor(dataVars$variable, levels = means$variable)
    },
    user = {
      dataVars$variable <- factor(dataVars$variable, levels = vars)
    },
    userRev = {
      dataVars$variable <- factor(dataVars$variable, levels = rev(vars))
    }
  )

  # Prepare data
  dataRegi <- dataVars %>%
    filter(.data$region != .env$mainReg)
  showNonMainRegs <- nrow(dataRegi) > 0

  if (!is.null(tot)) {
    dataTot <- dataModel %>%
      filter(.data$region == .env$mainReg, .data$variable == .env$tot) %>%
      droplevels()

    if (showNonMainRegs) {
      dataTotRegi <- dataModel %>%
        filter(.data$region != .env$mainReg, .data$variable == .env$tot) %>%
        droplevels()
    }
  } else {
    dataTot <- NULL
    dataTotRegi <- NULL
  }

  # Common label for y-axis.
  lcp <- if (is.null(tot)) gsub("\\|$", "", attr(shorten_legend(vars, identical_only = TRUE), "front")) else gsub(" pCap$", "", tot)
  label <- paste0(lcp, " (", paste0(levels(dataVars$unit), collapse = ","), ")")

  # Create plots.
  # plotAreaMain: area plot for each scenario, main region
  # no total line plotted here, can be added below
  plotAreaMain <- dataVars %>%
    filter(.data$region == .env$mainReg) %>%
    droplevels() %>%
    mipArea(scales = scales, total = FALSE, ylab = lcp) +
    ylab(NULL) +
    theme(legend.position = "none")

  # plotBarsMain: bar plot grouped by year, main region
  plotBarsMain <- bind_rows(dataVars, dataTot) %>%
    filter(.data$region == .env$mainReg, .data$period %in% .env$yearsBarPlot) %>%
    droplevels() %>%
    mipBarYearData(ylab = lcp, tot = tot) +
    ylab(NULL)


  if (showNonMainRegs) {
    plotBarsMain <- plotBarsMain + theme(legend.position = "none")

    # plotBarsRegi: bar plot for each region
    data.regi.bars <- bind_rows(dataRegi, dataTotRegi) %>%
      filter(.data$period %in% .env$yearsBarPlot)
    plotBarsRegi <- data.regi.bars %>%
      droplevels() %>%
      mipBarYearData(ylab = lcp, tot = tot) +
      ylab(NULL) +
      guides(fill = guide_legend(reverse = TRUE, ncol = 3))

    # plotAreaRegi: area plot for each scenario and region
    # no total line plotted here, can be added below
    plotAreaRegi <- dataRegi %>%
      droplevels() %>%
      mipArea(scales = scales, total = FALSE, ylab = lcp,
              stack_priority = if (is.null(tot)) c("variable", "region") else "variable") +
      guides(fill = guide_legend(reverse = TRUE))
  } else {
    plotBarsMain <- plotBarsMain +
      guides(fill = guide_legend(reverse = TRUE, ncol = 3))
  }

  # Add black lines in area plots from variable tot if provided.
  if (!is.null(tot)) {
    plotAreaMain <- plotAreaMain +
      geom_line(
        data = dataTot,
        mapping = aes(.data$period, .data$value),
        size = 1.3
      )

    if (showNonMainRegs) {
      dataTotRegi$scenario <- dataTotRegi$identifier
      plotAreaRegi <- plotAreaRegi +
        geom_line(
          data = dataTotRegi,
          mapping = aes(.data$period, .data$value),
          size = 1.3
        )
    }
  }

  if (showNonMainRegs) {
    return(list(
      showNonMainRegs = showNonMainRegs,
      p1 = list(plotAreaMain, plotBarsMain, plotBarsRegi, label),
      p2 = plotAreaRegi
    ))
  } else {
    return(list(
      showNonMainRegs = showNonMainRegs,
      p1 = list(plotAreaMain, plotBarsMain, label)
    ))
  }
}
