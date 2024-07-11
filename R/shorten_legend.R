#' Shorten legend names to a given length
#'
#' 1. If ylab is specified, the function just returns everything after the ylab.
#' 2. If identical_only = TRUE is specified, it removes identical parts in the name, independent of maxchar.
#' 3. If maxchar is specified, the function will first try to return vector as it is, then remove elements
#' which are identical between all elements and finally cut the end of the character vectors so that it
#' fits the given maxchar setting. Underscores will be replaced with empty spaces for further processing
#' @param x           A character vector or a factor vector that should be shortened
#' @param maxchar     Maximum number of characters allowed
#' @param identical_only If set to TRUE identical parts in the name will be removed,
#' independent of the character length (maxchar will be ignored!)
#' @param ylab If specified this part will be removed, independent of maxchar and identical_only
#' @param sep A vector of characters which should be interpreted as separators
#' @param unit A vector of characters with units, pasted to ylab
#' @author Jan Philipp Dietrich, Oliver Richters
#'
#' @examples
#' a <- c("Model Scenario_BLUB", "Model Scenario_BLA", "Model Scenario_BLA_BLA_BLUB")
#'
#' # do nothing
#' shorten_legend(a, 30)
#' # remove identical parts
#' shorten_legend(a, 15)
#' # or ...
#' shorten_legend(a, identical_only=TRUE)
#' # cutoff end of string
#' shorten_legend(a, 5)
#' # cut off the first part as explicitly specified
#' shorten_legend(a, ylab = "Model Scenario")
#' @export

shorten_legend <- function(x, maxchar = 20, identical_only = FALSE,  # nolint: object_name_linter.
                           ylab = NULL, sep = c(" ", "|", "_"), unit = NULL) {
  if (is.na(maxchar)) {
    warning("Maxchar was set to NA. Set maxchar back to default value (20).")
    maxchar <- 20
  }

  if (is.factor(x)) {
    x <- droplevels(x)
    tmp <- shorten_legend(levels(x), maxchar = maxchar, identical_only = identical_only,
                          ylab = ylab, sep = sep, unit = unit)
    levels(x) <- tmp
    attr(x, "front") <- attr(tmp, "front")
    attr(x, "back") <- attr(tmp, "back")
    attr(x, "ylab") <- attr(tmp, "ylab")
    return(x)
  }

  if (! is.null(ylab)) {
    # shorten labels by variable part of ylab
    ylabvariable <- magclass::unitsplit(ylab)$variable
    x_tmp <- gsub("^\\|", "", gsub(ylabvariable, "", x, fixed = TRUE))
    # avoid empty labels
    x <- ifelse(x_tmp == "", x, x_tmp)
  } else {
    sep_or <- paste0("\\", sep, collapse = "|")
    sep_no <- paste0("[^", paste0("\\", sep, collapse = ""), "]")
    reg_backsplit <- paste0("^(.*)(", sep_or, ")(", sep_no, "*)$")
    reg_frontsplit <- paste0("^(", sep_no, "*)(", sep_or, ")(.*)$")

    front <- NULL
    back <- NULL

    if (max(nchar(x)) > maxchar || identical_only) {
      #cut beginning
      while (length(unique(sub(reg_frontsplit, "\\1", x))) == 1 && all(grepl(sep_or, x))) {
        front <- paste0(front, sub(reg_frontsplit, "\\1\\2", x[1]))
        x <- sub(reg_frontsplit, "\\3", x)
      }
      attr(x, "front") <- front
      if (max(nchar(x)) > maxchar || identical_only) {
        #cut end
        back <- NULL
        while (length(unique(sub(reg_backsplit, "\\3", x))) == 1 && all(grepl(sep_or, x))) {
          back <- paste0(sub(reg_backsplit, "\\2\\3", x[1]), back)
          x <- sub(reg_backsplit, "\\1", x)
        }
        attr(x, "back") <- back
      }
    }
    # derive ylab and unit based on stuff that was cut away
    ylab <- gsub("  ", " ", paste0(c(sub(".$", "", front), back), collapse = "|... "))
  }
  # add unit
  if (! is.null(unit)) {
    unit <- unique(as.character(unit))
    ylab <- paste0(ylab, " (", paste0(unit, collapse = " | "), ")")
  }
  attr(x, "ylab") <- ylab

  if (max(nchar(x)) <= maxchar || identical_only) return(x)
  return(substring(x, 1, maxchar))
}
