#' getPlotData
#'
#' Get ready-to-plot data from one or more gdx files.
#'
#' @param symbolName The name of a symbol to be extracted from gdx.
#' @param pathToGdx  Path to one or more gdx files or a a path to a folder with fulldata gdx files. If multiple paths
#'                   are provided each one represents data after a specific iteration. The order of paths should match
#'                   iteration order, e.g. pathToGdx[1] should hold data for the first iteration, pathToGdx[2] for the
#'                   second iteration etc. If the path to a folder is given the fulldata gdx files in it are used.
#' @param ...        Additional arguments passed to gdxrrw::rgdx.
#' @return A data frame with data from the given gdx file(s). If multiple gdx files are provided an additional
#'         "iteration" column is added. The iteration value will be 1 for data rows from the first gdx, 2 for the second
#'         etc. The last column will always be the actual value column called <symbolName>.
#' @author Pascal Führlich
#' @seealso \code{\link{mipIterations}}, \code{\link{dataframeFromGdx}}
#' @export
getPlotData <- function(symbolName, pathToGdx = ".", ...) {
  stopifnot(
    length(symbolName) == 1,
    length(pathToGdx) > 0,
    all(file.exists(pathToGdx)),
    length(pathToGdx) == 1 || all(endsWith(pathToGdx, ".gdx"))
  )

  # if pathToGdx is path to a folder: set pathToGdx to the relevant fulldata gdx files in that folder
  if (length(pathToGdx) == 1 && !endsWith(pathToGdx, ".gdx")) {
    gdxFiles <- Sys.glob(file.path(pathToGdx, "fulldata_*.gdx"))
    if (length(gdxFiles) > 1) {
      fileNumber <- sub("fulldata_", "", basename(gdxFiles), fixed = TRUE)
      fileNumber <- sub(".gdx", "", fileNumber, fixed = TRUE)
      pathToGdx <- gdxFiles[order(as.integer(fileNumber))]
    } else {
      pathToGdx <- file.path(pathToGdx, "fulldata.gdx")
      stopifnot(file.exists(pathToGdx))
    }
  }

  # read one or more gdx files
  if (length(pathToGdx) == 1) {
    plotData <- dataframeFromGdx(symbolName, pathToGdx, ...)
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

      gdxContent <- dataframeFromGdx(symbolName, pathToGdx[[i]], ...)
      gdxContent["iteration"] <- i
      plotData <- rbind(plotData, gdxContent)
    }
  }

  plotData["iteration"] <- as.integer(plotData[["iteration"]])

  # move actual value column (called <symbolName>) to the end
  valueColumnIndex <- which(names(plotData) == symbolName)
  stopifnot(identical(length(valueColumnIndex), 1L))
  plotData <- plotData[c(seq_along(plotData)[-valueColumnIndex], valueColumnIndex)]
  return(plotData)
}
