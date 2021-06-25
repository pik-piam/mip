#' plotIterations
#'
#' Creates interactive line plots using ggplot and plotly. Creates one plot for each combination of values in columns
#' not plotted via this function's arguments. If the special value "year" is passed as xAxis, color, slider or facets a
#' list of possible column names representing years (e.g. "ttot", "tall", "t_all") is checked, the first one in names(x)
#' is used.
#'
#' @param plotData      A data frame. Use mip::getPlotData to get a ready-to-plot data frame from one or more gdx files.
#' @param returnGgplots If FALSE (the default) show interactive plotly plots with slider support. Set to TRUE to return
#'                      ggplots which can be customized, but are not interactive. To re-enable slider support and
#'                      interactivity run lapply(ggplots, plotly::ggplotly) after customizing the ggplots.
#' @param maxPlots      Return at most this many plots.
#' @param xAxis         A string from names(x), defining which column is plotted on the x-axis of the plots. Must not be
#'                      NULL.
#' @param color         A string from names(x), defining which column is plotted as color. If NULL color is not used.
#' @param slider        A string from names(x), defining which column is plotted as a slider. The slider requires
#'                      plotly. If NULL no slider is used.
#' @param facets        A string from names(x), defining which column is used for grouping. A small plot (facet) is
#'                      shown for each group. If NULL facets are not used.
#' @return A list of plotly plots, if returnGgplots is TRUE a list of ggplots
#' @author Pascal Führlich
#' @seealso \code{\link{getPlotData}}
#' @importFrom ggplot2 ggplot aes_string geom_line ylab facet_wrap ggtitle
#' @importFrom plotly ggplotly
#' @export
mipIterations <- function(plotData, returnGgplots = FALSE, maxPlots = 20L,
                          xAxis = "year", color = NULL, slider = "iteration", facets = "all_regi") {
  nonNullArgs <- Filter(Negate(is.null), c(xAxis, color, slider, facets))
  if (any(!(nonNullArgs %in% names(plotData) | nonNullArgs == "year"))) {
    stop(
      "The following columns were not found in the given plot data: ",
      paste0(nonNullArgs[!(nonNullArgs %in% names(plotData) | nonNullArgs == "year")], collapse = ", ")
    )
  } else if (anyDuplicated(nonNullArgs) != 0) {
    stop("Each column can only be mapped to one aesthetic, but there are duplicate arguments: ",
         paste0(nonNullArgs, collapse = ", "))
  }

  getYearDomainName <- function(aesthetic) {
    # take the first of the following possible year domain names that is in names(x)
    yearDomainNames <- c(
      "year", "ttot", "tall", "t_all", "t", "tsu", "opTimeYr", "opTime5", "t0", "t_input_gdx",
      "t_interpolate", "t_magiccttot", "t_magicc", "t_extra"
    )
    yearDomainName <- yearDomainNames[yearDomainNames %in% names(plotData)][1]
    if (is.null(yearDomainName)) {
      stop("Could not find year domain, please specify arguments to plotIterations explicitly.")
    }
    if (!identical(yearDomainName, "year")) {
      message('Using "', yearDomainName, '" instead of "year" for ', aesthetic)
    }
    return(yearDomainName)
  }

  if (identical(xAxis, "year")) {
    xAxis <- getYearDomainName("xAxis")
  } else if (identical(color, "year")) {
    color <- getYearDomainName("color")
  } else if (identical(slider, "year")) {
    slider <- getYearDomainName("slider")
  } else if (identical(facets, "year")) {
    facets <- getYearDomainName("facets")
  }

  # int is plotted more appropriately than factor when plotting e.g. years
  plotData[xAxis] <- as.integer(as.character(plotData[[xAxis]]))

  aestheticsArgs <- list(x = xAxis, y = "value")
  if (!is.null(color)) {
    aestheticsArgs <- c(aestheticsArgs, list(color = color))
    plotData[color] <- as.factor(plotData[[color]])
  }
  if (!is.null(slider)) {
    aestheticsArgs <- c(aestheticsArgs, list(frame = slider))
    if (!returnGgplots) {
      message("Make sure to run lapply(ggplots, plotly::ggplotly) to show the slider.")
    }
  }

  # all combinations of values of columns not plotted (not mapped to x/y/color etc.)
  plottedColumns <- c(xAxis, color, slider, facets, "value")
  unplottedCombinations <- unique(plotData[!(names(plotData) %in% plottedColumns)])
  unplottedCombinations <- lapply(split(unplottedCombinations, seq_len(nrow(unplottedCombinations))), as.list)

  if (length(unplottedCombinations) > maxPlots) {
    warning("Generating ", maxPlots, " plots instead of ", length(unplottedCombinations),
            ", run mip::mipIterations(..., maxPlots = ", length(unplottedCombinations), ") to catch them all.\n")
    unplottedCombinations <- unplottedCombinations[seq_len(maxPlots)]
  } else if (length(unplottedCombinations) == 0) {
    unplottedCombinations <- list(list())
  }

  # create a plot for each combination of unplotted values (not mapped to an aesthetic)
  plots <- lapply(unplottedCombinations, function(unplottedCombination) {
    # keep only rows corresponding to unplottedCombination
    x <- Reduce(function(filteredData, index) {
      return(filteredData[filteredData[[names(unplottedCombination)[[index]]]] == unplottedCombination[[index]], ])
    }, seq_along(unplottedCombination), plotData)

    heading <- attr(plotData, "symName")
    if (length(unplottedCombination) > 0) {
      heading <- paste(heading, substring(paste0(list(lapply(unplottedCombination, as.character))), 5))
    }

    plot <- ggplot(x, do.call(aes_string, aestheticsArgs)) +
      geom_line() +
      ggtitle(heading) +
      ylab("") # clear y-label, because the column name "value" is not helpful
    if (!is.null(facets)) {
      # create small plot for each region with different y scales, always show all facets, even if empty
      plot <- plot + facet_wrap(facets, scales = "free_y", drop = FALSE)
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
