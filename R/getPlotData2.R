#' getPlotData2
#'
#' Get ready-to-plot data from gdx files.
#'
#' @param filePaths A vector of paths to gdx files, each representing the state of a model after an iteration.
#' @param ... Arguments passed to gdx::readGDX, usually symbol names.
#' @return A data frame with the following columns: iteration (integer), year (integer), region, others extracted by
#' gdx::readGDX(...), symbol and value. According to the order of filePaths each file gets an iteration number. In the
#' resulting data frame all data rows from a file have that number as iteration value.
#' @author Pascal Führlich
#' @importFrom gdx readGDX
#' @export
getPlotData2 <- function(filePaths, ...) {
  plotData <- NULL
  for (i in seq_along(filePaths)) {
    if (!grepl(paste0("[^0-9]0*", i, "[^0-9]"), filePaths[[i]])) {
      warning('WARNING: "', filePaths[[i]], '" should contain data for iteration ', i,
              ' but that path does not contain "', i,
              '" - are file paths missing/ordered incorrectly? Consider using gtools::mixedsort\n')
    }
    gdxContent <- readGDX(filePaths[[i]], ..., restore_zeros = FALSE, format = list("simple"))

    # convert to dataframe, add iteration and symbol value and append to plotData
    for (j in seq_along(gdxContent)) {
      x <- magclass::as.data.frame(gdxContent[[j]], rev = 2)

      names(x)[names(x) == ".value"] <- "value"
      x$symbol <- names(gdxContent)[[j]]
      x$iteration <- as.integer(i)

      # reorder columns to match mip::getPlotData
      otherColumns <- if (ncol(x) > 5) {
        3:(ncol(x) - 3)
      } else {
        vector(mode = "integer")
      }
      x <- x[, c(
        ncol(x),      # iteration
        2,            # year
        1,            # region
        otherColumns, # others...
        ncol(x) - 1,  # symbol
        ncol(x) - 2   # value
      )]

      plotData <- tryCatch(rbind(plotData, x),
        error = function(error) {
          warning(paste0(
            "WARNING: Cannot merge with previous data, skipping symbol ", names(gdxContent)[[j]],
            "\nreason: the previous columns\n", paste(colnames(plotData), collapse = ", "), "\nare incompatible to\n",
            paste(colnames(x), collapse = ", "), "\noriginal error message: ", error
          ))
          return(plotData)
        }
      )
    }
  }
  return(plotData)
}
