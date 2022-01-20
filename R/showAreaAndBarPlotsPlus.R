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
