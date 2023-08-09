#' plotIterations
#'
#' Creates interactive line plots using ggplot and plotly. Creates one plot for each combination of values in columns
#' not plotted via this function's arguments. If the special value "year" is passed as xAxis, color, slider or facets a
#' list of possible column names representing years (e.g. "ttot", "tall", "t_all") is checked, the first one in names(x)
#' is used.
#'
#' @param plotData      A data frame. The actual value column must be the last. Use mip::getPlotData to get a
#'                      ready-to-plot data frame from one or more gdx files.
#' @param returnGgplots If FALSE (the default) show interactive plotly plots with slider support. Set to TRUE to return
#'                      ggplots which can be customized, but are not interactive. To re-enable slider support and
#'                      interactivity run lapply(ggplots, plotly::ggplotly) after customizing the ggplots.
#' @param xAxis         A string from names(x), defining which column is plotted on the x-axis of the plots. Must not be
#'                      NULL.
#' @param color         A string from names(x), defining which column is plotted as color. If NULL color is not used.
#' @param slider        A string from names(x), defining which column is plotted as a slider. The slider requires
#'                      plotly. If NULL no slider is used.
#' @param facets        A string from names(x), defining which column is used for grouping. A small plot (facet) is
#'                      shown for each group. If NULL facets are not used.
#' @return A list of plotly plots, if returnGgplots is TRUE a list of ggplots instead
#' @author Pascal Führlich
#' @seealso \code{\link{getPlotData}}
#' @importFrom ggplot2 ggplot aes_string geom_line ylab facet_wrap ggtitle
#' scale_color_gradientn theme_bw theme element_blank
#' @importFrom grDevices rainbow
#' @importFrom plotly ggplotly
#' @importFrom utils tail
#' @export
mipIterations <- function(plotData, returnGgplots = FALSE,
                          xAxis = "year", color = NULL, slider = "iteration", facets = "region") {
  nonNullArgs <- Filter(Negate(is.null), c(xAxis, color, slider, facets))
  if (any(!(nonNullArgs %in% c(names(plotData), "year", "region")))) {
    stop(
      "The following columns were not found in the given plot data: ",
      paste0(nonNullArgs[!(nonNullArgs %in% c(names(plotData), "year", "region"))], collapse = ", ")
    )
  } else if (anyDuplicated(nonNullArgs) != 0) {
    stop("Each column can only be mapped to one aesthetic, but there are duplicate arguments: ",
         paste0(nonNullArgs, collapse = ", "))
  }

  handleSpecialValues <- function(aesthetic, argumentValue) {
    findDomainName <- function(domainNames) {
      domainName <- domainNames[domainNames %in% names(plotData)][1]
      if (is.null(domainName)) {
        stop("Could not find ", argumentValue, " domain, please specify arguments to plotIterations explicitly.")
      }
      if (!identical(domainName, argumentValue)) {
        message('Using "', domainName, '" instead of "', argumentValue, '" for ', aesthetic)
      }
      return(domainName)
    }

    if (identical(argumentValue, "year")) {
      return(findDomainName(c("year", "ttot", "tall", "t_all", "t", "tsu", "opTimeYr", "opTime5", "t0", "t_input_gdx",
                              "t_interpolate", "t_magiccttot", "t_magicc", "t_extra")))
    } else if (identical(argumentValue, "region")) {
      return(findDomainName(c("region", "all_regi", "regi", "alt_regions", "ext_regi", "RCP_regions_world_bunkers",
                              "RCP_regions_world")))
    } else {
      return(argumentValue)
    }
  }

  xAxis <- handleSpecialValues("xAxis", xAxis)
  color <- handleSpecialValues("color", color)
  slider <- handleSpecialValues("slider", slider)
  facets <- handleSpecialValues("facets", facets)

  # int is plotted more appropriately than factor when plotting e.g. years
  plotData[xAxis] <- as.integer(as.character(plotData[[xAxis]]))

  valueColumnName <- tail(names(plotData), 1)
  if (!is.double(plotData[[valueColumnName]])) {
    warning("Expected the type of the last column (", valueColumnName, ") to be double, but it is ",
            typeof(plotData[[valueColumnName]]))
  }
  aestheticsArgs <- list(x = xAxis, y = valueColumnName)
  if (!is.null(color)) {
    aestheticsArgs <- c(aestheticsArgs, list(color = color))
  }
  if (!is.null(slider)) {
    aestheticsArgs <- c(aestheticsArgs, list(frame = slider))
    if (returnGgplots) {
      message("Make sure to run lapply(ggplots, plotly::ggplotly) to show the slider.")
    }
  }

  # all combinations of values of columns not plotted (not mapped to x/y/color etc.)
  plottedColumns <- c(xAxis, color, slider, facets, valueColumnName)

  if (length(plottedColumns) == ncol(plotData)) {
    unplottedCombinations <- list(list())
  } else {
    unplottedCombinations <- unique(plotData[!(names(plotData) %in% plottedColumns)])
    unplottedCombinations <- lapply(split(unplottedCombinations, seq_len(nrow(unplottedCombinations))), as.list)
  }

  # create a plot for each combination of unplotted values (not mapped to an aesthetic)
  plots <- lapply(unplottedCombinations, function(unplottedCombination) {
    # keep only rows corresponding to unplottedCombination
    x <- Reduce(function(filteredData, index) {
      return(filteredData[filteredData[[names(unplottedCombination)[[index]]]] == unplottedCombination[[index]], ])
    }, seq_along(unplottedCombination), plotData)

    heading <- tail(names(plotData), 1)
    if (length(unplottedCombination) > 0) {
      heading <- paste(heading, substring(paste0(list(lapply(unplottedCombination, as.character))), 5))
    }

    plot <- ggplot(x, do.call(aes_string, aestheticsArgs)) +
      geom_line() +
      ggtitle(heading) +
      theme_bw() +
      theme(strip.background = element_blank())
    if (!is.null(facets)) {
      # by default create a small plot for each region; always show all facets, even if empty
      plot <- plot + facet_wrap(facets, drop = FALSE)
    }
    if (!is.null(color) && is.numeric(plotData[[color]])) {
      plot <- plot + scale_color_gradientn(colours = rainbow(5, v = 0.8))
    }
    return(plot)
  })
  names(plots) <- lapply(plots, function(plot) plot$label$title)
  if (!returnGgplots) {
    # return plotly plots instead of ggplots
    plots <- lapply(plots, ggplotly)
  }
  return(plots)
}
