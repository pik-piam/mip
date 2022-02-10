#' Returns plot styles for given entities
#'
#' Returns a named vector (using entity names) with style codes (e.g. colors)
#' for given entities.
#'
#' @param ... One or more strings or a vector of strings with names of entities
#'   (regions, variable names, etc.). Units in brackets "(US$2005/GJ)" will be
#'   ignored. If left empty all available entities will be used
#' @param out Switch defining which type of plot style you want to get:
#'   Currently you can choose between "color", "legend" and "all" (the latter
#'   will return a dataframe with all available plot styles)
#' @param unknown Optional data frame defining plot styles for unknown entities.
#'   A default color map will be used for unknown entities if nothing is
#'   provided here
#' @param plot If TRUE plots with all given entities and their colors will be
#'   produced (to produce plots with all available entities leave the \code{...}
#'   entry empty!)
#' @param verbosity Set to 1 if you want to know for which unknown entities
#'   plotstyle brewed colors
#' @param regexp If \code{TRUE}, match entities as regular expressions. Matching
#'   entities are expanded, non-matching entities are returned as the original
#'   expression.  Does not generate default color maps. Implies \code{plot =
#'   FALSE} and \code{verbosity = 0}.
#' @return Plot styles for given entities
#' @section Colors for unknown entities:
#' \if{html}{\figure{colors.png}{options: width="100\%"}}
#' @author David Klein, Jan Philipp Dietrich
#' @seealso \code{\link{plotstyle.add}}
#' @examples
#' entities <- c("AFR", "AAA", "AFR", "UNKNOWN_ELEMENT2")
#' plotstyle(entities)
#' unknown <- data.frame(row.names = c("AAA", "UNKNOWN_ELEMENT2"),
#'                    color = c("#123456", "#345678"),
#'                    legend = c("l_AAA", "l_UNKNOWN_ELEMENT2"))
#' plotstyle(entities, unknown = unknown)
#' plotstyle(entities, out = "legend")
#' plotstyle(entities, out = "all")
#'
#' # search for all 'Final Energy Biomass' entities
#' plotstyle("^Final Energy\\|.*Biomass", regexp = TRUE)
#'
#' # search for all three-letter entities (a.k.a. regions)
#' plotstyle("^[A-Z]{3}$", regexp = TRUE, out = "all")
#' @export
#' @importFrom utils read.csv2
#' @importFrom grDevices rgb
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_bar coord_flip theme element_blank labs aes
#' @importFrom grDevices colorRampPalette

plotstyle <- function(..., out = "color", unknown = NULL, plot = FALSE, verbosity = getOption("plotstyle.verbosity"),
                      regexp = FALSE) {

  luplot <- list()
  luplot$plotstyle <- read.csv2(
    system.file("extdata", "plotstyle.csv", package = "mip"),
    stringsAsFactors = FALSE,
    row.names = 1)

  if (is.null(out)) {
    out <- "color"
  } else if (!(out %in% c(names(luplot$plotstyle), "all"))) {
    stop('Unknown style type "', out, '"!')
  }

  # make sure that luplot$plotstyle is of type data.frame
  class(luplot$plotstyle) <- "data.frame"

  # choose plot colors for given entities
  entity <- c(...)
  if (is.null(entity)) {
    entity <- row.names(luplot$plotstyle)
  } else {
    entity[is.na(entity)] <- "NA"
    entity <- unlist(lapply(strsplit(entity, " \\("), function(x) x[1]))
  }

  uqEntity <- unique(entity)
  res <- luplot$plotstyle[uqEntity, ]
  row.names(res) <- uqEntity

  # ignore rest of function if regexp is used
  if (regexp) {
    res <- data.frame()
    for (r in uqEntity) {
      # get rows that match the regular expression
      indices <- which(grepl(r, row.names(luplot$plotstyle)))
      # add rows found
      if (0 < length(indices)) {
        newRows <- luplot$plotstyle[indices, ]
      } else {
        # if none are found, add an 'NA'-row
        newRows <- luplot$plotstyle[paste(row.names(luplot$plotstyle),
                                           collapse = ""), ]
        # with the regular expression as name
        row.names(newRows) <- r
      }
      res <- rbind(res, newRows)
    }

    if ("all" == out) {
      return(res)
    } else {
      return(setNames(getElement(res, out), rownames(res)))
    }
  }

  # count unknown entities, i.e. count rows that have NA only,
  # i.e. where number of columns is the same as number of NAs
  ina <- rowSums(is.na(res)) == NCOL(res)
  nna <- sum(ina)

  # replace NA
  if (nna != 0) {
    if (is.null(unknown)) {
      # The following list contains 19 easily distinguishable colors. If you
      # need n < 20 colors, choose the first n colors from that list. If you
      # need more colors, colorRampPalette() is used to create interpolated
      # colors. This may result in a poor choice of colors. A warning is
      # produced, as the elements of a plot with these color may not be
      # distinguishable by color.
      tmpcols <- c(
        "#e6194B", "#3cb44b", "#4363d8", "#f58231", "#911eb4", "#469990",
        "#9A6324", "#800000", "#808000", "#000075", "#f032e6", "#ffd610",
        "#404040", "#42d4f4", "#bfef45", "#B0B0B0", "#dcbeff", "#aaffc3",
        "#fabed4")
      if (nna < 20) {
        tmpcols <- tmpcols[1:nna]
      } else {
        warning("Need to choose 20 colors or more. The colors will be difficult to distinguish.")
      }
      colorPalette <- colorRampPalette(tmpcols)
      if (!is.null(verbosity)) {
        cat("Brewed colors for", nna, "unknown entities:\n")
        cat(row.names(res)[ina], sep = "\n")
      }
      suppressWarnings(res$color[is.na(res$color)] <- colorPalette(nna))
      # replace NA in legends with row names (= entity name)
      res$legend[is.na(res$legend)] <- row.names(res[is.na(res$legend), ])
    } else {
      if (out == "all") {
        if (!all(names(unknown) %in% names(luplot$plotstyle))) {
          stop("There are elements in names(unknown) that are not in names(plotstyle)!")
        }
        for (n in names(unknown)) {
          res[[n]][is.na(res[[n]])] <- as.character((unknown[[n]][1:nna]))
        }
      } else if (!(out %in% names(unknown))) {
        stop('Style type "', out, '" is not existing in argument "unknown"!')
      } else {
        rowsWithoutData <- rownames(subset(res, is.na(res[[out]])))
        res[rowsWithoutData, out] <- as.character(unknown[rowsWithoutData, out])
      }
    }
  }

  if (plot) {
    df <- data.frame(x = uqEntity, c = res$color)
    # prevent ggplot from sorting it alphabetical by giving order explicitly here
    df$x <- factor(df$x, levels = rev(uqEntity))

    ncol    <- 30 # color bars per page
    pagemax <- ceiling(length(res$color) / ncol) # number of pages
    for (page in 1:pagemax) {
      # start and end index for respective page
      from <- (page - 1) * ncol + 1
      to   <- min(page * ncol, length(res$color))
      # create data frame
      x <- rownames(res)[from:to]
      c <- res$color[from:to]
      df <- data.frame(x = x, c = c)
      # prevent ggplot from sorting it alphabetically by giving order explicitly here
      # using original order of rownames. Reversing it because the bar plot reverses it again
      # To yield the correct mapping between colors and labels the colors have to be also reversed
      df$x <- factor(df$x, levels = rev(x))
      # create bar plot
      p1 <- ggplot(data = df, aes(x = x)) + geom_bar(stat = "count", fill = rev(df$c)) + coord_flip() +
               theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
               labs(title = paste0("Color bars (plot ", page, " of ", pagemax, ")"))

      print(p1)
    }

  }

  res <- res[entity, ]

  # select the output data from res according to "out"
  if (out != "all") {
    res <- res[[out]]
    names(res) <- entity
  }
  return(res)
}
