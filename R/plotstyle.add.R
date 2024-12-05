#' Adds plot styles locally to plotstyle.csv
#'
#' Adds plot styles locally to plotstyle object and returns a dataframe with all
#' plotstyles including the added data.
#' However, it does NOT change the "./inst/extdata/plotstyle.csv".
#' To add new entities to "./inst/extdata/plotstyle.csv" please open the file in
#' your editor and add or change values by hand.
#' By default plotstyles of already existing entities will not be changed.
#' Only new entities will be added. Use the \code{replace} switch to replace existing styles.
#' If you want to keep the legend text or the color of an already existing entity and
#' only replace one of the two values, use the string "keep" for the value you want to keep.
#'
#' @param entity Vector of strings with names of entities (regions, variable names, etc.)
#' @param legend Vector of strings with legend names of entities.
#' @param color Vector of strings containing hexadecimal color codes.
#' @param marker optional Vector of strings with marker codes.
#' @param linestyle optional Vector of strings containing linestyle codes .
#' @param replace Logical (default FALSE) indicating whether existing data should be replaced with new data.
#' @author David Klein, Jan Philipp Dietrich
#' @seealso \code{\link{plotstyle}}
#' @examples
#' \dontrun{
#' plotstyle.add("AFR", "Africa", "#000000")
#' }
#' \dontrun{
#' plotstyle.add("AFR", "keep", "#FFFFFF")
#' }
#' \dontrun{
#' plotstyle.add("AFR", "keep", "keep", marker = 20, replace = TRUE)
#' }
#' @importFrom utils read.csv2 write.csv2
#' @export
plotstyle.add <- function(entity, legend, color, # nolint
                          marker = NULL, linestyle = NULL, replace = FALSE) {
  # in no data is given for marker and linestyle fill with NA
  if (is.null(marker)) marker <- rep(NA, time = length(entity))
  if (is.null(linestyle)) linestyle <- rep(NA, time = length(entity))

  # Check if all parameters are of the same length
  a <- diff(range(c(length(entity), length(legend), length(color), length(marker), length(linestyle))))
  if (a > .Machine$double.eps^0.5) {
    stop("Entities, legend names, and colors have to be of the same length.")
  }

  if (file.exists(system.file("extdata", "plotstyle_local.csv", package = "mip"))) {
    styleFile <- system.file("extdata", "plotstyle_local.csv", package = "mip")
    message("Loading local plotsyles from ", styleFile)
  } else {
    styleFile <- system.file("extdata", "plotstyle.csv", package = "mip")
  }

  plotstyles <- read.csv2(styleFile, stringsAsFactors = FALSE, row.names = 1)
  class(plotstyles) <- "data.frame"

  # find existing entries
  dup <- which(entity %in% rownames(plotstyles))

  # create data.frame with new data
  newdata <- data.frame(
    row.names = entity, legend = legend, color = color, marker = marker,
    linestyle = linestyle, stringsAsFactors = FALSE
  )

  # add new data to existing data and replace if wanted
  if (length(dup) == 0) {
    # if no duplicates are found between old and new data append new data
    plotstyles <- rbind(plotstyles, newdata)
  } else if (replace) {
    # add new data and replace existing data
    # keep existing data where new data contains "keep"
    # keep legend
    i <- rownames(subset(newdata, legend == "keep"))
    newdata[i, ]$legend <- plotstyles[i, ]$legend
    # keep color
    i <- rownames(subset(newdata, color == "keep"))
    newdata[i, ]$color <- plotstyles[i, ]$color
    # keep marker
    i <- rownames(subset(newdata, marker == "keep"))
    newdata[i, ]$marker <- plotstyles[i, ]$marker
    # keep linestyle
    i <- rownames(subset(newdata, linestyle == "keep"))
    newdata[i, ]$linestyle <- plotstyles[i, ]$linestyle

    plotstyles[entity, ] <- newdata
  } else {
    # append new data without replacing duplicates in existing data
    cat("Element", entity[dup], "already exist. It has not been added.
        Please use replace=TRUE to replace it. All other data has been added\n")
    plotstyles <- rbind(plotstyles, newdata[-dup, ])
  }

  message(
    "NOTE: This function adds new elements to inst/extdata/plotstyle_local.csv locally only! ",
    "To make it available for everyone, please replace replace the file inst/extdata/plotstyle.csv ",
    "with your local plotstyle file and create a pull request to mip.",
    "In order to use your local style file when calling mip::plotstyle(), set the option 'plotsyle.local' to TRUE"
  )

  write.csv2(plotstyles, file = file.path(system.file("extdata", package = "mip"), "plotstyle_local.csv"), quote = FALSE)

  return(plotstyles)
}
