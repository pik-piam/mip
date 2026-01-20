#' Show Area and Bar Plots Utilizing '+'-Notation
#'
#' Automatically infers disaggregation of a variable using the '+'-notation and
#' calls \code{\link{showAreaAndBarPlots}}.
#'
#' The function requires \code{data} to have a character column named
#' \code{varplus} containing variable names that use the '+'-notaion. The
#' function searches for values in \code{varplus} that start with \code{tot}
#' followed by \code{"|"}, \code{plusNum}-times \code{"+"}, and \code{"|"}.
#' These variables are then used in a call to \code{\link{createAreaAndBarPlots}}.
#'
#' @inheritDotParams createAreaAndBarPlotsPlus
#' @return \code{NULL} is returned invisible.
#' @examples
#' \dontrun{
#' options(mip.yearsBarPlot = c(2010, 2030, 2050, 2100))
#' options(mip.mainReg = "World")
#' data <- as.quitte(data)
#' showAreaAndBarPlotsPlus(data, "SE|Liquids")
#' }
#' @export
showAreaAndBarPlotsPlus <- function(...) {
  plots <- createAreaAndBarPlotsPlus(...)
  for (plot in plots) {
    showPlot(plot)
    cat("\n\n")
  }
  return(invisible(NULL))
}

#' Create Area and Bar Plus Plots
#'
#' Creates the plots for \code{\link{showAreaAndBarPlotsPlus}}
#' @param tot A single string. A total value to be shown in the area plots.
#' @param plusNum A single number. Number of "+"symbols for disaggregation.
#' @inheritParams createAreaAndBarPlots
createAreaAndBarPlotsPlus <- function(
  data, tot, plusNum = 1, fill = FALSE,
  mainReg = getOption("mip.mainReg"),
  yearsBarPlot = getOption("mip.yearsBarPlot"),
  scales = "free_y"
) {

  data <- as.quitte(data)

  # Validate function arguments.
  # fill, mainReg, yearsBarPlot are not used directly and validated in showAreaAndBarPlots()
  stopifnot((is.character(tot) && length(tot) == 1) || is.null(tot))
  stopifnot(is.numeric(plusNum) && length(tot) == 1)
  stopifnot("varplus" %in% names(data))
  stopifnot(is.character("varplus"))

  allVarsPlus <- unique(data$varplus)
  prefix <- paste0(tot, "|", strrep("+", plusNum), "|")
  varsPlus <- allVarsPlus[startsWith(allVarsPlus, prefix)]

  vars <- piamutils::deletePlus(varsPlus)

  return(layoutAreaAndBarPlots(
    createAreaAndBarPlots(data, vars,
      tot = tot, fill = fill, mainReg = mainReg,
      yearsBarPlot = yearsBarPlot, scales = scales
    )
  ))
}
