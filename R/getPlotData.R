#' getPlotData
#'
#' Get ready-to-plot data from one or more gdx files.
#'
#' @param pathToGdx Path to one or more gdx files. If multiple paths are provided each one represents data after a
#' specific iteration. The order of paths should match iteration order, e.g. pathToGdx[1] should hold data for
#' the first iteration, pathToGdx[2] for the second iteration etc.
#' @param symbolName The name of a symbol to be extracted from gdx.
#' @param compress Logical, passed to gdxrrw::rgdx.param. Ensures factor levels are equal to unique elements.
#' @param ... Additional arguments passed to gdxrrw::rgdx.param.
#' @return A data frame with data from the given gdx file(s). The column called <symbolName> is renamed to "value".
#' The symbol name is stored in attr(plotData, "symName"). If multiple gdx files are provided an additional "iteration"
#' column is added. The iteration value will be 1 for all data rows from the first gdx, 2 for the second etc.
#' @author Pascal Führlich
#' @seealso \code{\link{plotIterations}}
#' @importFrom gdxrrw rgdx.param
#' @export
getPlotData <- function(pathToGdx, symbolName, compress = TRUE, ...) {
  stopifnot(length(pathToGdx) > 0, all(file.exists(pathToGdx)), length(symbolName) == 1)
  if (length(pathToGdx) == 1) {
    plotData <- rgdx.param(pathToGdx, symbolName, compress = compress, ...)
  } else {
    plotData <- NULL
    for (i in seq_along(pathToGdx)) {
      if (!grepl(paste0("[^0-9]0*", i, "[^0-9]"), pathToGdx[[i]])) {
        warning(
          'WARNING: "', pathToGdx[[i]], '" should contain data for iteration ', i,
          ' but that path does not contain "', i,
          '" - are file paths missing/ordered incorrectly? Consider using gtools::mixedsort\n'
        )
      }

      gdxContent <- rgdx.param(pathToGdx[[i]], symbolName, compress = compress, ...)
      gdxContent$iteration <- i
      plotData <- rbind(plotData, gdxContent)
    }
  }

  names(plotData)[names(plotData) == symbolName] <- "value"
  plotData$iteration <- as.integer(plotData$iteration)
  attr(plotData, "domains") <- NULL
  attr(plotData, "domInfo") <- NULL
  return(plotData)
}
