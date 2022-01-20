# Generates a warning if some of the variable names in the character vector vars
# are not entries of the variables column of the data frame data.
warnMissingVars <- function(data, vars) {
  available <- vars %in% unique(data$variable)
  missingVars <- vars[!available]
  if (length(missingVars) > 0)
    warning("Variables not found: ", paste(missingVars, collapse = ", "),
            call. = FALSE)
}

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

# Helper function to extract the legend of a ggplot object.
getLegend <- function(plt) {
  tmp <- ggplot_gtable(ggplot_build(plt))
  legIdx <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(legIdx) == 0) return(NULL)
  tmp$grobs[[legIdx[1]]]
}

# Helper function. Changes the value of variables given in numerators by dividing
# by denominator and multiplying conversionFactor. Sets unit of these variables to newUnit.
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

longestCommonPrefix <- function(x) {
  for (i in 1:min(nchar(x))) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) return(substr(x[1], 1, i - 1))
  }
  substr(x[1], 1, i)
}


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
  stopifnot(is.numeric(data$period))
  stopifnot(is.numeric(data$value))
  
  # Check type of optional columns.
  if ("gdp" %in% names(data)) 
    stopifnot(is.numeric(data$gdp))
  if ("variablePlus" %in% names(data)) 
    stopifnot(is.character(data$variablePlus) || is.factor(data$variablePlus))
  
  return(data)
}
