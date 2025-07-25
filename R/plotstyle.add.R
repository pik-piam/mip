#' Adds plot styles locally to plotstyle.csv
#'
#' Adds plot styles locally as a per-session setting and returns a dataframe
#' with all plot styles including the added data.
#'
#' By default, plot styles of already existing entities will not be changed.
#' Only new entities will be added.  Use the `replace` switch to replace
#' existing styles.  If you want to keep the legend text or the color of an
#' already existing entity and only replace one of the two values, use the
#' string `"keep"` for the value you want to keep.
#'
#' To change plot styles permanently, use the `writefile` argument to write the
#' updated plot styles to a `.csv` file, use it to update
#' `./inst/extdata/plotstyle.csv`, and build a new package version.
#'
#' @md
#' @param entity Vector of strings with names of entities (regions, variable
#'     names, etc.)
#' @param legend Vector of strings with legend names of entities.
#' @param color Vector of strings containing hexadecimal color codes.
#' @param marker optional Vector of strings with marker codes.
#' @param linestyle optional Vector of strings containing linestyle codes.
#' @param replace Logical (default `FALSE`) indicating whether existing data
#'     should be replaced with new data.
#' @param write_file If `TRUE`, the updated plot styles are write to the file
#'     `plotstyles.csv` in the current working directory.
#'
#'
#' @author David Klein, Jan Philipp Dietrich
#'
#' @seealso [plotstyle()]
#'
#' @examples
#' \dontrun{plotstyle.add("AFR", "Africa", "#000000")}
#' \dontrun{plotstyle.add("AFR", "keep",   "#FFFFFF")}
#' \dontrun{plotstyle.add("AFR", "keep",   "keep", marker = 20, replace = TRUE)}
#'
#' @importFrom utils write.csv2
#' @export
plotstyle.add <- function(entity, legend, color, marker = NULL,
                          linestyle = NULL, replace = FALSE, write_file = FALSE)
{

  # in no data is given for marker and linestyle fill with NA
  if (is.null(marker))    marker    <- rep(NA, times= length(entity))
  if (is.null(linestyle)) linestyle <- rep(NA, times= length(entity))

  # Check if all parameters are of the same length
  a <- diff(range(c(length(entity),length(legend),length(color),length(marker),length(linestyle))))
  if (a > .Machine$double.eps ^ 0.5) {
    stop("Entities, legend names, and colors have to be of the same length.")
  }

  # read plotstyles from plotstyles()-internal cache
  plotstyles <- getElement(get("cache", envir = environment(plotstyle), inherits = FALSE), "ps")

  class(plotstyles) <- "data.frame"

  # find existing entries
  dup <- which(entity %in% rownames(plotstyles))

  # create data.frame with new data
  newdata <- data.frame(row.names=entity,legend=legend,color=color,marker=marker,linestyle=linestyle,stringsAsFactors=FALSE)

  # add new data to existing data and replace if wanted
  if (length(dup)==0) {
    # if no duplicates are found between old and new data append new data
    plotstyles <- rbind(plotstyles,newdata)
  } else if (replace) {
    # add new data and replace existing data
    # keep existing data where new data contains "keep"
    # keep legend
    i <- rownames(subset(newdata,legend=="keep"))
    newdata[i,]$legend <- plotstyles[i,]$legend
    # keep color
    i <- rownames(subset(newdata,color=="keep"))
    newdata[i,]$color <- plotstyles[i,]$color
    # keep marker
    i <- rownames(subset(newdata,marker=="keep"))
    newdata[i,]$marker <- plotstyles[i,]$marker
    # keep linestyle
    i <- rownames(subset(newdata,linestyle=="keep"))
    newdata[i,]$linestyle <- plotstyles[i,]$linestyle

    plotstyles[entity,] <- newdata
  } else {
    # append new data without replacing duplicates in existing data
    warning("Element", entity[dup], "already exist. It has not been added. ",
            "Please use replace=TRUE to replace it. All other data has been",
            " added")
    plotstyles <- rbind(plotstyles,newdata[-dup,])
  }

  # save plot styles to plotstyle()-internal cache
  cache <- get("cache", envir = environment(plotstyle), inherits = FALSE)
  cache$ps <- plotstyles

  if (isTRUE(write_file)) {
    write.csv2(x = plotstyles, file = 'plotstyles.csv', quote = FALSE)
  }

  return(plotstyles)
}
