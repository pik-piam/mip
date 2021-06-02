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
#' @export
getPlotData <- function(filePaths, ...) {
  plotData <- NULL
  for (i in seq_along(filePaths)) {
    gdxContent <- readGDX(filePaths[[i]], ..., restore_zeros = FALSE)

    # ensure we deal with a list even if ...length() == 1
    if (is.magpie(gdxContent)) {
      gdxContent <- list(gdxContent)
      names(gdxContent) <- ..1
    }

    # convert to dataframe, add iteration and variable value and append to plotData
    for (j in seq_along(gdxContent)) {
      df <- magclass::as.data.frame(gdxContent[[j]], rev = 2)
      df$variable <- names(gdxContent)[[j]]
      df$iteration <- as.factor(i)
      plotData <- rbind(plotData, df)
    }
  }
  return(plotData)
}
