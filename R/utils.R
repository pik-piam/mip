#' Warn If Variables Are Missing
#'
#' Generates a warning if some of the variable names in \code{vars} vars are not
#' entries of the variables-column of \code{data}.
#'
#' @param data A quitte object.
#' @param vars A character vector.
#' @export
#' @return Returns \code{NULL} invisibly.
#' @export

warnMissingVars <- function(data, vars) {
  available <- vars %in% levels(data$variable)
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
#' @export
checkGlobalOptionsProvided <- function(optNames, envir=rlang::caller_env()) {
  for (on in optNames) {
    if (is.null(get0(on, envir = envir))) {
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
  legIdx <- which(vapply(tmp$grobs, function(x) x$name, "") == "guide-box")
  if (length(legIdx) == 0) return(NULL)
  tmp$grobs[[legIdx[1]]]
}

#' Calculate Ratios for Quitte Objects.
#'
#' Changes the value of variables given in \code{numerators} by dividing by
#' \code{denominator} and multiplying \code{conversionFactor}. Sets \code{unit}
#' of these variables to \code{newUnit}.
#'
#' @param data A quitte object.
#' @param numerators A character vector. Entries in the variable column of data.
#' @param denominator A single string. An entry in the variable column of data.
#' @param newUnit A single string.
#' @param conversionFactor A single numerical value.
#' @return A quitte object with changed values and new unit. Unused levels are dropped.
#' @importFrom rlang .data .env
#' @export
calculateRatio <- function(
  data, numerators, denominator, newUnit = "1", conversionFactor = 1
) {
  denom <- data %>%
    filter(.data$variable == .env$denominator) %>%
    rename(denom_value = .data$value) %>%
    select(.data$model, .data$scenario, .data$region, .data$period, .data$denom_value)
  res <- data %>%
    filter(.data$variable %in% .env$numerators) %>%
    left_join(denom, by = c("model", "scenario", "region", "period")) %>%
    mutate(value = .data$value / .data$denom_value * .env$conversionFactor,
           unit = factor(.env$newUnit)) %>%
    droplevels()
  return(res)
}

#' Longest Common Prefix
#'
#' @param x A character vector.
#' @return A single string. The longest common prefix of \code{x}.
#' @export
longestCommonPrefix <- function(x) {
  stopifnot(is.character(x))
  if (length(x) == 0 || any(is.na(x))) return(NA_character_)
  n <- min(nchar(x))
  if (n == 0) return("")
  for (i in 1:n) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) return(substr(x[1], 1, i - 1))
  }
  return(substr(x[1], 1, n))
}



#' add identifier based on model and scenario names
#' @param x A quitte object
#' @return A factor. If more than one model but only one scenario, use model.
#' If more than one scenario but only one model, use scenario. Else, combine them.
#' @export
identifierModelScen <- function(x) {
  x <- droplevels(quitte::as.quitte(x))
  if (nlevels(x$model) > 1 && nlevels(x$scenario) == 1) {
    x$identifier <- x$model
    attr(x$identifier, "deletedinfo") <- paste("Scenario:", levels(x$scenario)[[1]])
  } else if (nlevels(x$model) == 1) {
    x$identifier <- x$scenario
    attr(x$identifier, "deletedinfo") <- paste("Model:", levels(x$model)[[1]])
  } else {
    x$identifier <- as.factor(paste(x$model, x$scenario))
  }
  return(x$identifier)
}
