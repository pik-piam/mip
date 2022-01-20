#' Show Area and Bar Plots Utilizing '+'-Notation
#'
#' Automatically infers disaggregation of a variable using the '+'-notaion and
#' calls \code{\link{showAreaAndBarPlots}}.
#'
#' The function requires \code{data} to have to column named \code{variablePlus}
#' containing variable names that use the '+'-notaion. The function searches for
#' values in \code{variablePlus} that start with \code{tot} followed by
#' \code{"|"}, \code{plusNum}-times \code{"+"}, and \code{"|"}. These variables
#' are then used in a call to \code{\link{showAreaAndBarPlots}}.
#'
#' @param tot A single string. A total value to be shown in the area plots.
#' @param plusNum A single number. Number of "+"symbols for disaggregation.
#' @inheritParams showAreaAndBarPlots
#' @return \code{NULL} is returned invisible.
#' @examples
#' \dontrun{
#' showAreaAndBarPlotsPlus(data, "SE|Liquids")
#' }
#' @export
showAreaAndBarPlotsPlus <- function(
  data, tot, plusNum = 1, fill = FALSE,
  mainReg = getOption("mip.mainReg"),
  yearsBarPlot = getOption("mip.yearsBarPlot")
) {
  
  # Validate function arguments. 
  # fill, mainReg, yearsBarPlot are not used directly and validated in showAreaAndBarPlots()
  data <- validateData(data, removeSuperfluousCols=TRUE)
  stopifnot((is.character(tot) && length(tot) == 1) || is.null(tot))
  stopifnot(is.numeric(plusNum) && length(tot) == 1)
  
  if (!"variablePlus" %in% names(data)) stop("Require 'variablePlus' column.")
  
  allVarsPlus <- unique(data$variablePlus)
  prefix <- paste0(tot, "|", str_dup("+", plusNum), "|")
  varsPlus <- allVarsPlus[str_starts(allVarsPlus, fixed(prefix))]
  vars <- str_replace_all(varsPlus, "\\|\\++\\|", "|")
  
  showAreaAndBarPlots(data, vars, tot, fill, mainReg, yearsBarPlot)
}
