#' plotIterations
#'
#' Creates a line plot using ggplot with the following mapping: x = iteration, y = value, color = year,
#' linetype = variable. Use mip::getPlotData to create a data frame from gdx, modify it to include all expected columns,
#' see documentation for parameter x. Copy-paste and modify the code of this function to create a plot tailored to your
#' specific case.
#'
#' @param x A data frame, columns with the following names are expected:
#' iteration (int), variable (chr or factor), .value (num). x[3] must represent region (chr or factor), x[4] must
#' represent year (int). Additional columns are ignored.
#' @return A ggplot
#' @author Pascal Führlich
#' @seealso \code{\link{getPlotData}}
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line ylab facet_wrap
#' @export
plotIterations <- function(x) {
  region <- names(x)[3]
  year <- names(x)[4]
  x[year] <- as.factor(x[[year]]) # convert the variable mapped to color as factor so it is plotted nicely

  plot <- ggplot(x, aes(
    x = .data$iteration,
    y = .data$.value,
    color = .data[[year]],
    linetype = .data$variable
  )) +
    geom_line() +
    ylab("") + # clear y-label, because the column name ".value" is not helpful
    facet_wrap(region, scales = "free_y") # create a small plot for each region, with different y scales
  message("To change the plot run `print(mip::plotIterations)`, copy-paste and modify. Run `library(plotly)` first.")
  return(ggplotly(plot)) # turn into interactive plot (click legend to disable some years, hover for tooltips, zoom)
}
