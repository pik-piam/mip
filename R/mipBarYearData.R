#' @title mipBarYearData
#' @description Function for plotting (bar-plot) MAgPIE objects and compare different scenarios,
#' on the x-axis for some time steps one bar for each scenario is generated
#'
#'
#' @param x Data to plot. Allowed data formats: magpie or quitte
#' @param ylab y-axis text
#' @param xlab x-axis text
#' @param title title appering at the top of the plot
#' @param colour Dimension to be colored, default: "Scenario"
#' @param scenario_markers Use markers to conserve space with long scenario
#'        names.  Symbols are either picked automatically (default), or can be
#'        passed as a named vector in the form of
#'        \code{c('scenario' = 'marker')}, where marker is a number between 1
#'        and 20, or a ggplot2 shape name
#'        (see \code{vignette("ggplot2-specs")}).  Set to \code{FALSE} to not
#'        use markers.
#' @author Lavinia Baumstark
#' @section Example Plot:
#' \if{html}{\figure{mipBarYearData.png}{example plot}}
#' \if{html}{\figure{mipBarYearData_oneRegi.png}{example plot}}
#' \if{html}{\figure{mipBarYearData_oneScenario.png}{example plot}}
#'
#' @examples
#' \dontrun{
#' plotCompBarYearData(EnInv, ylab = "Energy Investments|Elec (billion US$2005/yr)",
#'                      colour = plotstyle(getNames(EnInv, dim = 2)))
#' }
#'
#' @importFrom magclass is.magpie
#' @importFrom ggplot2 ggplot aes_ guides guide_legend scale_x_continuous
#'             ggtitle geom_col scale_shape_manual
#' @importFrom dplyr %>% mutate filter inner_join group_by summarise select n sym arrange
#' @importFrom tidyr crossing unite
#' @importFrom quitte order.levels
#' @export
#


mipBarYearData <- function(x, colour = NULL, ylab = NULL, xlab = NULL, title = NULL,
                           scenario_markers = TRUE) { #nolint
  scenarioMarkers <- scenario_markers
  x <- as.quitte(x)

  if (length(unique(x$model)) > 1) {
    stop("this plot can only deal with data that have only one model")
  }

  if (!is.integer(x$period)) {
    stop("this plot can only deal with data that have integer periods")
  }

  # calculate y-axis label
  x$variable <- shorten_legend(x$variable, identical_only = TRUE)

  if (is.null(ylab)) {
     ylab <- paste0(sub(".$", "", attr(x$variable, "front")), attr(x$variable, "back"))
     # add unit
     unit <- unique(as.character(x$unit))
     ylab <- paste0(ylab, " (", paste0(unit, collapse = " | "), ")")
  }

  # add dummy-dimension for space between the time-steps
  xpos <- crossing(period   = unique(x$period),
                   scenario = factor(c(levels(x$scenario), "\x13"))) %>%
    order.levels(scenario = c(levels(x$scenario), "\x13")) %>%
    arrange(!!sym("period"), !!sym("scenario")) %>%
    mutate(xpos = 1:n()) %>%
    filter("\x13" != !!sym("scenario")) %>%
    droplevels()

  x <- x %>%
    inner_join(
      xpos,

      c("scenario", "period")
    )

  if (scenarioMarkers) {
    yMarker <- crossing(
      x %>%
        group_by(!!sym("region"), !!sym("xpos")) %>%
        summarise(top    = sum(pmax(0, !!sym("value"))),
                  bottom = sum(pmin(0, !!sym("value")))) %>%
        summarise(top    = max(!!sym("top")),
                  bottom = min(!!sym("bottom"))) %>%
        mutate(
          y = !!sym("bottom") - 0.05 * (!!sym("top") + !!sym("bottom"))) %>%
        select(-"top", -"bottom"),

      xpos
    )
  }

  if (scenarioMarkers) {
    scenarioMarkers <- stats::setNames((1:20)[seq_along(unique(x$scenario))],
                                 levels(x$scenario))
  }

  # calculate positions of period labels
  if (any(scenarioMarkers)) {
    xpos <- xpos %>%
      group_by(!!sym("period")) %>%
      summarise(xpos = mean(!!sym("xpos")))
  }

  if (is.null(colour)) {
    colour <- plotstyle(levels(x$variable))
  }

  # make plot
  p <- ggplot() +
    geom_col(data = x,
             mapping = aes(x = !!sym("xpos"), y = !!sym("value"),
                           fill = !!sym("variable"))) +
    scale_fill_manual(values = colour, name = NULL,
                      guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~region, scales = "free_y") +
    labs(x = xlab, y = ylab, title = title) +
    theme(legend.position = "bottom")

  # add markers
  if (any(scenarioMarkers)) {
    p <- p +
      scale_x_continuous(breaks = xpos$xpos,
                         labels = xpos$period) +
      geom_point(data = yMarker,
                 mapping = aes(x = !!sym("xpos"), y = !!sym("y"),
                               shape = !!sym("scenario")),
                 size = 1.5) +
      scale_shape_manual(values = scenarioMarkers, name = NULL) +
      theme(legend.box = "vertical")
  } else {
    p <- p +
      scale_x_continuous(breaks = xpos$xpos,
                         labels = xpos %>%
                           unite(!!sym("label"), !!sym("scenario"),
                                 !!sym("period"), sep = " ") %>%
                           getElement("label")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  return(p)
}
