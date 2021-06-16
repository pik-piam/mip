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
  # TODO documentation
  getYearDomainName <- function() {
    # take the first of the following possible year domain names that is in names(x)
    yearDomainNames <- c(
      "year", "ttot", "tall", "t_all", "t", "tsu", "opTimeYr", "opTime5", "t0", "t_input_gdx",
      "t_interpolate", "t_magiccttot", "t_magicc", "t_extra"
    )
    yearDomainName <- yearDomainNames[yearDomainNames %in% names(plotData)][1]
    if (is.null(yearDomainName)) {
      stop("Could not find year domain, please specify arguments to plotIterations explicitly.")
    }
    return(yearDomainName)
  }

  if (identical(xAxis, "year")) xAxis <- getYearDomainName()
  if (identical(color, "year")) color <- getYearDomainName()
  if (identical(slider, "year")) slider <- getYearDomainName()
  if (identical(facets, "year")) facets <- getYearDomainName()

  # int is plotted more appropriately than factor when plotting e.g. years
  plotData[xAxis] <- as.integer(as.character(plotData[[xAxis]]))

  plot <- ggplot(plotData, aes(
    x = .data[[xAxis]],
    y = .data$value,
    # color = .data[[color]], # TODO
    frame = .data[[slider]]
  )) +
    geom_line() +
    ggtitle(attr(plotData, "symName")) +
    ylab("") + # clear y-label, because the column name "value" is not helpful
    facet_wrap(facets, scales = "free_y") # create a small plot for each region, with different y scales
  message(
    "Run `library(plotly)` first, use ggplotly(<ggplot>) to make plots interactive. ",
    "To change the plots run `print(mip::plotIterations)`, copy-paste and modify."
  )
  return(plot)
}
