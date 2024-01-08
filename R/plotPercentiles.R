#' Comparison plots show 50th percentile of user selected variables as obtained from different scenario runs. If
#' available in the data, ribbon plots will also show the 33th - 67th percentile in a darker color and the 5th – 95th
#' percentile in a lighter color. *Note*: the 5th, 33th, 67th and 95th percentile must be given in the provided data set
#' as the percentiles are not computed

#' @author Tonn Rueter
#' @param df `quitte` style data frame containing all variables for each scenario. In the quitte data frame all
#'    percentiles must be given as individual variables. Manipulate input data frame such that all percentiles
#'    of a given quantity are transformed to individual columns.
#' @param scenarios Character vector contains names of the desired scenarios. If none, all scenarios will be displayed
#' @param variables Character vector contains names of the desired variables. If none, all variables will be displayed
#'    Variable names in the quitte data frame need to follow the format "Any|Variable|50.0th Percentile". When
#'    selecting variables for display only use the "Any|Variable"-prefix and omit the "X-th Percentile"-suffix
#' @importFrom dplyr filter mutate vars
#' @importFrom reshape2 melt
#' @importFrom stringr str_extract
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot geom_line geom_ribbon facet_wrap facet_grid theme ylab
#' @export
plotPercentiles <- function(df, scenarios = NULL, variables = NULL) {

  # In the quitte data frame all percentiles are given as individual variables
  # Manipulate input data frame such that all percentiles of a given quantity
  # are transformed to individual columns. Variable names in the quitte data
  # frame follow the format "Any|Variable|5.0th Percentile". The regular
  # expressions below divide the variable name into the prefix and the
  # percentile specifier
  data <- df %>%
    mutate(
      "percentile" = stringr::str_extract(.data$variable, "[^\\|]+?$"),
      "variable"   = gsub("\\|[^\\|]+$", "", .data$variable)
    ) %>%
    pivot_wider(
      names_from = "percentile",
      values_from = "value"
    )

  # Check which scenarios/variabes are available
  uniqueScenarios <- unique(data$scenario)
  uniqueVariables <- unique(data$variable)

  # Check which function parameters have been provided and default to unique
  # values from the data frame in case none have
  theseScenarios <- if (is.null(scenarios)) {
    uniqueScenarios
  } else if (allItemsAvailable(scenarios, uniqueScenarios, warn = TRUE)) {
    scenarios
  } else {
    stop("Provided scenario is missing in data")
  }
  theseVariables <- if (is.null(variables)) {
    uniqueVariables
  } else if (allItemsAvailable(variables, uniqueVariables, warn = TRUE)) {
    variables
  } else {
    stop("Provided variable is missing in data")
  }

  # Set up the plot
  p <- ggplot()

  # Fill plot by filtering for the requested variables and scenarios
  for (thisVariable in theseVariables) {
    for (thisScenario in theseScenarios) {
      plotData <- filter(data, .data$variable == thisVariable & .data$scenario == thisScenario)
      p <- p +
        geom_line(
          data = plotData, aes(x = .data$period, y = get("50.0th Percentile"))
        ) +
        geom_ribbon(
          data = plotData, aes(x = .data$period, ymin = get("33.0th Percentile"), ymax = get("67.0th Percentile")),
          fill = "#68788a", alpha = 0.5
        ) +
        geom_ribbon(
          data = plotData, aes(x = .data$period, ymin = get("5.0th Percentile"), ymax = get("95.0th Percentile")),
          fill = "#68788a", alpha = 0.2
        )
    }
  }

  # Depending on the function parameters, plots need to be arranged
  if (length(theseScenarios) == 1) {
    # Plots all parameters for a given scenario. Y-axes need to be independent
    p <- p +
      facet_wrap(vars(.data$variable), scales = "free_y", ncol = 1) +
      theme(axis.title.x = element_blank()) +
      ylab(unique(data$unit))
  } else if (length(theseVariables) == 1) {
    # Plots a given parameter for all scenarios. Lock y-axes to improve comparison
    p <- p +
      facet_wrap(vars(.data$scenario)) +
      theme(axis.title.x = element_blank()) +
      ylab(unique(data$unit))
  } else {
    # Using facet grid when multiple variables in multiple scenarios are compared
    p <- p +
      facet_grid(.data$variable ~ .data$scenario, scales = "free_y") +
      theme(axis.title.x = element_blank()) +
      ylab(unique(data$unit))
  }

  return(p)
}

allItemsAvailable <- function(selection, available, warn = FALSE) {
  for (item in selection) {
    if (!(item %in% available)) {
      if (warn) warning(paste0("'", item, "' missing in available data"))
      return(FALSE)
    }
  }
  return(TRUE)
}
