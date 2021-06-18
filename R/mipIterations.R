#' plotIterations
#'
#' Creates a line plot using ggplot with the following mapping: x = iteration, y = value, color = year,
#' linetype = symbol. Use mip::getPlotData to create a data frame from gdx, modify it to include all expected columns
#' (see documentation for parameter x) then run mip::plotIterations. Copy-paste and modify the code of this function to
#' create a plot tailored to your specific case.
#'
#' @param x A data frame, columns with the following names are expected:
#' iteration (int), symbol (chr or factor), value (num). x[3] must represent region (chr or factor), x[4] must
#' represent year (int). Additional columns are ignored.
#' @return A ggplot
#' @author Pascal Führlich
#' @seealso \code{\link{getPlotData}}
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line ylab facet_wrap ggtitle
#' @export
mipIterations <- function(plotData, xAxis = "year", color = NULL, slider = "iteration", facets = "all_regi") {
  # TODO documentation, document special value year
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
    message("Use lapply(plots, plotly::ggplotly) to show the slider.")
  }

  # all combinations of values of columns not plotted (not mapped to x/y/color etc.)
  unplottedCombinations <- unique(plotData[, !(names(plotData) %in% c(xAxis, color, slider, facets, "value"))])
  unplottedCombinations <- lapply(split(unplottedCombinations, seq(nrow(unplottedCombinations))), as.list)

  # create a plot for each combination of unplotted values (not mapped to an aesthetic)
  result <- lapply(unplottedCombinations, function(unplottedCombination) {
    # keep only rows corresponding to unplottedCombination
    x <- Reduce(function(filteredData, index) {
      return(filteredData[filteredData[[names(unplottedCombination)[[index]]]] == unplottedCombination[[index]], ])
    }, seq_along(unplottedCombination), plotData)

    title <- paste(attr(plotData, "symName"), substring(paste0(list(lapply(unplottedCombination, as.character))), 5))

    plot <- ggplot(x, do.call(aes_string, aestheticsArgs)) +
      geom_line() +
      ggtitle(title) +
      ylab("") # clear y-label, because the column name "value" is not helpful
    if (!is.null(facets)) {
      # create small plot for each region with different y scales, always show all facets, even if empty
      plot <- plot + facet_wrap(facets, scales = "free_y", drop = FALSE)
    }
    return(plot)
  })
  return(result)
}
