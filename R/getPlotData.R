#' getPlotData
#'
#' Get ready-to-plot data from gdx files.
#'
#' @param filePaths A vector of paths to gdx files, each representing the state of a model after an iteration.
#' @param ... Arguments passed to gdx::readGDX, usually variable names.
#' @return A data frame with the variables Region, Year (converted to integer), the ones extracted by gdx::readGDX(...)
#' and an additional Iteration variable. According to the order in which filePaths are provided each file gets an
#' iteration number. In the resulting data frame all data rows from a file have that number as Iteration value.
#' @author Pascal Führlich
#' @importFrom gdx readGDX
#' @examples
#' \dontrun{
#' ggplot(
#'   getPlotData(Sys.glob("output/C_SSP2-Base-rem-*/fulldata.gmx"), "p30_pebiolc_pricemag"),
#'   aes(
#      x = .data$Year,
#'     y = .data$Value,
#'     color = .data$Iteration,
#'     group = .data$Iteration
#'   )
#' ) +
#'   geom_line() +
#'   facet_wrap(~Region, scales = "free_y")
#' }
#' @export
getPlotData <- function(filePaths, ...) {
  plotData <- NULL
  for (i in seq_len(length(filePaths))) {
    gdxContent <- as.data.frame(readGDX(filePaths[[i]], ...))
    gdxContent$Iteration <- as.factor(i) # nolint
    gdxContent$Year <- as.integer(levels(gdxContent$Year))[gdxContent$Year] # nolint
    plotData <- rbind(plotData, gdxContent)
  }
  return(plotData)
}
