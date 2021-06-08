#' plotIterations
#'
#' TODO
#'
#' @param x A data frame as returned by mip::getPlotData. Columns with the following names are expected:
#' iteration (int), variable (chr or factor), .value (num). x[3] must represent region (chr or factor), x[4] must
#' represent year (int). Additional columns are ignored.
#' @return A ggplot
#' @author Pascal Führlich
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line ylab facet_wrap
#' @export
plotIterations <- function(x) {
  # run library(plotly) when copy-pasting this code
  region <- names(x)[3]
  year <- names(x)[4]
  x[year] <- as.factor(x[[year]]) # convert the variable mapped to color as factor so it is plotted nicely

  plot <- ggplot(x, aes(x = .data$iteration,
                        y = .data$.value,
                        color = .data[[year]],
                        linetype = .data$variable)) +
    geom_line() +
    ylab("") + # clear y-label, because the column name ".value" is not helpful
    facet_wrap(region, scales = "free_y") # create a small plot for each region, with different y scales
  print(plotIterations) # print the code of this function
  return(ggplotly(plot)) # turn into interactive plot (click legend to disable some years, hover for tooltips, zoom)
}
