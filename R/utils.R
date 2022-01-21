#' Warn If Variables Are Missing
#'
#' Generates a warning if some of the variable names in \code{vars} vars are not
#' entries of the variables-column of \code{data}.
#'
#' @param data A data frame with a column named \code{variable}.
#' @param vars A character vector.
#' @return Returns \code{NULL} invisibly.
warnMissingVars <- function(data, vars) {
  available <- vars %in% unique(data$variable)
  missingVars <- vars[!available]
  if (length(missingVars) > 0)
    warning("Variables not found: ", paste(missingVars, collapse = ", "),
            call. = FALSE)
  return(invisible(NULL))
}

#' Check Whether Global Options Are Provided
#'
#' The function checks whether a variable of the name \code{optNames} is
#' available. If not a warning message is produced, indicating how this variable
#' can be set using \code{options()}.
#'
#' @param optNames A character vector. The names of the variables.
#' @param envir The environment where to look for the variables.
#' @return Returns \code{NULL} invisibly.
checkGlobalOptionsProvided <- function(optNames, envir=rlang::caller_env()) {
  for (on in optNames) {
    if (is.null(get0(on, envir=envir))) {
      stop(
        on, " must be provided, ",
        "either as a function argument or ",
        "as a global option via options(mip.", on, "=<value>).",
        call. = FALSE)
    }
  }
  return(invisible(NULL))
}

#' Get Legend of a ggplot.
#' 
#' The function extracts the legend of a ggplot object.
#' 
#' @param plt A ggplot object.
#' @return The legend of \code{plt} or \code{NULL} if no legend was found.
getLegend <- function(plt) {
  tmp <- ggplot_gtable(ggplot_build(plt))
  legIdx <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(legIdx) == 0) return(NULL)
  tmp$grobs[[legIdx[1]]]
}

#' Calculate Ratios for mif Data Frame.
#'
#' Changes the value of variables given in \code{numerators} by dividing by
#' \code{denominator} and multiplying \code{conversionFactor}. Sets \code{unit}
#' of these variables to \code{newUnit}.
#'
#' @param data A mif data frame with columns \code{variable}, \code{model},
#'   \code{scenario}, \code{region}, \code{period}.
#' @return A mif data frame with changed values and new unit.
calacuateRatio <- function(
  data, numerators, denominator, newUnit = "1", conversionFactor = 1
) {
  data %>%
    filter(variable == denominator) %>%
    rename(denom_value = value) %>%
    select(model, scenario, region, period, denom_value) ->
    denom
  data %>%
    filter(variable %in% numerators) %>%
    left_join(denom) %>%
    mutate(value = value / denom_value * conversionFactor,
           unit = newUnit)
}

#' Longest Common Prefix
#' 
#' @param x A character vector. 
#' @return A single string. The longest common prefix of \code{x}.
longestCommonPrefix <- function(x) {
  for (i in 1:min(nchar(x))) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) return(substr(x[1], 1, i - 1))
  }
  substr(x[1], 1, i)
}


# TODO: Use is.quitte()?
#' Validate mif Data Frame
#'
#' Checks whether \code{data} is a data frame and has the right columns to be a
#' mif data frame. Throws error if not ok.
#'
#' @param data The object to be checked.
#' @param removeSuperfluousCols A single truth-value. Should unrecognized
#'   columns be removed? Set this to \code{TRUE} to make sure that the data
#'   frame does not contain columns that create unforeseen results when using
#'   non-standard evaluation in \code{dplyr}-functions.
#' @return Returns \code{data} (with less columns if required).
validateData <- function(data, removeSuperfluousCols=FALSE) {
  stopifnot(is.data.frame(data))
  requiredCols <- c("model", "scenario", "region", "variable", "unit", "period", "value", "variable")
  optionalCols <- c("gdp", "variablePlus")
  superfluousCols <- setdiff(names(data), c(requiredCols, optionalCols))
  stopifnot(requiredCols %in% names(data))
  if (length(superfluousCols) > 0) {
    warning(
      "There are superfluousCols columns in data object: ", 
      paste0(superfluousCols, sep=", "))
    if (removeSuperfluousCols)
      data <- data[setdiff(names(data), superfluousCols)]
  }
  
  # Check type of required columns.
  stopifnot(is.character(data$model) || is.factor(data$model))
  stopifnot(is.character(data$scenario) || is.factor(data$scenario))
  stopifnot(is.character(data$region) || is.factor(data$region))
  stopifnot(is.character(data$variable) || is.factor(data$variable))
  stopifnot(is.character(data$unit) || is.factor(data$unit))
  stopifnot(is.numeric(data$period) || methods::is(data$period, "POSIXct"))
  stopifnot(is.numeric(data$value))
  
  # Check type of optional columns.
  if ("gdp" %in% names(data)) 
    stopifnot(is.numeric(data$gdp))
  if ("variablePlus" %in% names(data)) 
    stopifnot(is.character(data$variablePlus) || is.factor(data$variablePlus))
  
  return(data)
}
