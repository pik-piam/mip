#' getPlotData
#'
#' Get ready-to-plot data from gdx files.
#'
#' @param pathToGdx Path to a single gdx file.
#' @param symbolNames A vector of names of symbols to be extracted from the gdx. For each symbol the first two domains
#' should represent iteration and year, they are converted to integer.
#' @param ... Additional arguments passed to gdxrrw::rgdx.param.
#' @return A data frame combined from data frames extracted by gdxrrw::rgdx.param, where an additional "symbol" column
#' is holding the corresponding symbol name from symbolNames. The last column is always holding the actual values and is
#' called "value".
#' @author Pascal Führlich
#' @seealso \code{\link{plotIterations}}
#' @importFrom gdxrrw rgdx.param
#' @export
getPlotData <- function(pathToGdx, symbolNames, ...) {
  factorToInt <- function(aFactor) {
    return(as.integer(levels(aFactor)[aFactor]))
  }

  reduceFunction <- function(combinedDataframe, symbolName) {
    x <- rgdx.param(pathToGdx, symbolName, ...)
    x[, 1] <- factorToInt(x[, 1]) # expecting column 1 to represent iteration
    x[, 2] <- factorToInt(x[, 2]) # expecting column 2 to represent year

    # add symbol column and rename symbol value column to just value so data for multiple symbols can be combined
    x$symbol <- symbolName
    names(x[symbolName]) <- "value"

    x[, c(which(x, "value"), ncol(x))] <- x[, c(ncol(x), which(x, "value"))] # swap value column to be last
    combinedDataframe <- tryCatch(
      rbind(combinedDataframe, x),
      error = function(error) {
        warning(paste0(
          "WARNING: Cannot merge with previous data, skipping symbol ", symbolName,
          "\nreason: the previous columns\n", paste(colnames(combinedDataframe), collapse = ", "),
          "\nare incompatible to\n", paste(colnames(x), collapse = ", "), "\noriginal error message: ", error
        ))
        return(combinedDataframe)
      }
    )
    return(combinedDataframe)
  }

  return(Reduce(reduceFunction, symbolNames, NULL))
}
