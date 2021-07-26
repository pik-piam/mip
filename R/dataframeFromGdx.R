#' dataframeFromGdx
#'
#' Read data for a given symbol from a gdx file and return it as a dataframe.
#'
#' This function is similar to gdxrrw::rgdx.param, but it also works for variables.
#'
#' @param symbolName The name of a symbol to be extracted from gdx.
#' @param pathToGdx Path to a gdx file.
#' @param ... Additional arguments passed to gdxrrw::rgdx.
#' @return A data frame with data about symbolName from the given gdx file. All columns are character vectors,
#'         except the last column which holds numeric values, because that is the format that rgdx returns.
#' @author Pascal Führlich
#' @seealso \code{\link{getPlotData}}
dataframeFromGdx <- function(symbolName, pathToGdx, ...) {
  if (!requireNamespace("gdxrrw", quietly = TRUE)) {
    stop('Package "gdxrrw" is required, please install it: ',
         "https://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r")
  }

  x <- gdxrrw::rgdx(pathToGdx, list(name = symbolName), ...)
  # UELs = Unique Elements Lists
  uels <- x[["uels"]] # a named list where name = domain and value = character vector of unique elements
  val <- x[["val"]] # 2D vector of indices into uels, except for last column which is the actual numeric value
  stopifnot(identical(length(uels), dim(val)[2] - 1L),
            identical(length(uels), length(x[["domains"]])))

  resolvedValues <- lapply(seq_along(uels), function(i) {
    return(uels[[i]][val[, i]]) # use indices from val to get actual values from corresponding unique element list
  })
  result <- do.call(data.frame, c(resolvedValues, list(val[, length(uels) + 1])))
  colnames(result) <- c(x[["domains"]], x[["name"]])

  stopifnot(identical(dim(val), dim(result)))
  return(result)
}
