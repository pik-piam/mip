#' @title mipBarYearData
#' @description Function for plotting (bar-plot) MAgPIE objects and compare different scenarios
#' or models, on the x-axis for some time steps one bar for each scenario/model is generated
#'
#'
#' @param x Data to plot. Allowed data formats: magpie or quitte
#' @param ylab y-axis text
#' @param xlab x-axis text
#' @param title title appearing at the top of the plot
#' @param colour Dimension to be colored, default: "Scenario"
#' @param scenario_markers Use markers to conserve space with long scenario
#'        names.  Symbols are either picked automatically (default), or can be
#'        passed as a named vector in the form of
#'        \code{c('scenario' = 'marker')}, where marker is a number between 1
#'        and 20, or a ggplot2 shape name
#'        (see \code{vignette("ggplot2-specs")}).  Set to \code{FALSE} to not
#'        use markers.
#' @param tot variable used to show the total on top of stacked bars
#' @author Lavinia Baumstark, Oliver Richters
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
#'             ggtitle geom_col scale_shape_manual theme_minimal theme geom_segment
#' @importFrom dplyr %>% mutate filter inner_join group_by summarise select n sym arrange
#' @importFrom tidyr crossing unite
#' @importFrom quitte order.levels
#' @export
#

mipBarYearData <- function(x, colour = NULL, ylab = NULL, xlab = NULL, title = NULL,
                           scenario_markers = TRUE,
                           tot = NULL) { #nolint
  scenarioMarkers <- scenario_markers
  data <- droplevels(as.quitte(x))
  if (! "identifier" %in% names(data)) data$identifier <- identifierModelScen(data)
  
  if (!is.integer(data$period)) {
    stop("this plot can only deal with data that have integer periods")
  }
  
  if (nrow(data) == 0) {
    warning("Quitte object is empty.")
    return()
  }
  
  # add dummy-dimension for space between the time-steps
  xpos <- crossing(period     = unique(data$period),
                   identifier = factor(c(levels(data$identifier), "\x13"))) %>%
    order.levels(identifier = c(levels(data$identifier), "\x13")) %>%
    arrange(!!sym("period"), !!sym("identifier")) %>%
    mutate(xpos = 1:n()) %>%
    filter("\x13" != !!sym("identifier")) %>%
    droplevels()

  data <- data %>%
    inner_join(
      xpos,
      c("identifier", "period")
    )

  if (!is.null(tot)) {
    dataTotal <- data %>% filter(.data$variable == tot)
    data <- data %>% filter(.data$variable != tot)
  }
  
  # if not given derive y-axis label, shorten variables accordingly
  data$variable <- shorten_legend(data$variable, ylab = ylab, identical_only = TRUE, unit = data$unit)
  ylab <- attr(data$variable, "ylab")
  
  if (scenarioMarkers) {
    yMarker <- crossing(
      data %>%
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
    scenarioMarkers <- stats::setNames((1:20)[seq_along(unique(data$identifier))],
                                       levels(data$identifier))
  }
  
  # calculate positions of period labels
  if (any(scenarioMarkers)) {
    xpos <- xpos %>%
      group_by(!!sym("period")) %>%
      summarise(xpos = mean(!!sym("xpos")))
  }
  
  if (is.null(colour)) {
    colour <- plotstyle(levels(data$variable), strip_units = FALSE)
  }
  
  # make plot
  p <- ggplot() +
    geom_col(data = data,
             mapping = aes(x = !!sym("xpos"), y = !!sym("value"),
                           fill = !!sym("variable"))) +
    scale_fill_manual(values = colour, name = NULL,
                      guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~region, scales = "free_y") +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (!is.null(tot)) { # add a dash representing the total of stacked bars
    p <- p + geom_segment(
      data = dataTotal,
      aes(x = !!sym("xpos") - 0.3, xend = !!sym("xpos") + 0.3,
          y = !!sym("value"), yend = !!sym("value")),
      color = "black", linewidth = 1.3
    )
  }
  
  # add markers
  if (any(scenarioMarkers)) {
    p <- p +
      scale_x_continuous(breaks = xpos$xpos,
                         labels = xpos$period) +
      geom_point(data = yMarker,
                 mapping = aes(x = !!sym("xpos"), y = !!sym("y"),
                               shape = !!sym("identifier")),
                 size = 1.5) +
      scale_shape_manual(values = scenarioMarkers, name = NULL) +
      theme(legend.box = "vertical",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
  } else {
    p <- p +
      scale_x_continuous(breaks = xpos$xpos,
                         labels = xpos %>%
                           unite(!!sym("label"), !!sym("identifier"),
                                 !!sym("period"), sep = " ") %>%
                           getElement("label")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  
  return(p)
}
