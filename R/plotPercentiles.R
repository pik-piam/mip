#' Comparison line plots with percentiles
#'
#' Line plots show median (50th percentile) of user selected variable(s) obtained from different scenario runs. If
#' available in the data, ribbon plots will also show the 33th - 67th percentile region in a darker color and the
#' 5th – 95th percentile region in a lighter color. Note: the 5th, 33th, 67th and 95th percentiles must be provided in
#' the data set as the percentiles are not computed
#'
#' @author Tonn Rüter
#' @param df The \code{quitte}-style data frame must contain all percentiles of the quantity of interest as individual
#'    variables (e.g. for atmospheric CO2 concentrations "Atmospheric Concentrations|CO2|50th Percentile",
#'    "Atmospheric Concentrations|CO2|33th Percentile", ..., must be present)
#' @param scenarios Character vector containing names of the desired scenarios. If \code{NULL}, all scenarios present in
#'    the data will be displayed
#' @param variables Character vector containing names of the desired variables. If \code{NULL}, all variables present in
#'    the data will be displayed. When selecting particular variables for display only use the "Any|Variable"-prefix and
#'    omit the "X-th Percentile"-suffix (e.g. for atmospheric CO2 concentrations write "Atmospheric Concentrations|CO2")
#' @examples
#' \dontrun{
#' # Plot atmospheric CO2 concentrations for all scenarios available in the data
#' p <- plotPercentiles(
#'   data,
#'   # Use variable name without "X-th Percentile"-suffix
#'   variables = c("AR6 climate diagnostics|Atmospheric Concentrations|CO2|MAGICCv7.5.3")
#' )
#' # Plot all available variables for selected scenarios
#' p <- plotPercentiles(data, scenarios = c("d_delfrag", "d_another"))
#' }
#' @section Example Plot:
#' \if{html}{\figure{plotPercentiles1.png}{Atmospheric CO2 concentrations for all scenarios available in the data}}
#' @importFrom dplyr filter mutate vars
#' @importFrom reshape2 melt
#' @importFrom stringr str_extract
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot geom_line geom_ribbon facet_wrap facet_grid theme ylab
#' @export
plotPercentiles <- function(df, scenarios = NULL, variables = NULL) {

  # In the quitte data frame all percentiles are given as individual variables. Manipulate input data frame such that
  # all percentiles of a given quantity are transformed to individual columns. Variable names in the quitte data frame
  # follow the format "Any|Variable|5.0th Percentile". The regular expressions below divide the variable name into the
  # prefix and the percentile specifier
  data <- df %>%
    mutate(
      "percentile" = stringr::str_extract(.data$variable, "[^\\|]+?$"),
      "variable"   = gsub("\\|[^\\|]+$", "", .data$variable)
    ) %>%
    pivot_wider(
      names_from = "percentile",
      values_from = "value"
    )

  # Check which scenarios/variables are available
  uniqueScenarios <- unique(data$scenario)
  uniqueVariables <- unique(data$variable)

  # Check which function parameters have been provided and default to unique values from the data frame in case none
  # have. If scenarios/variables have been provided by user, check whether they are available in the data
  if (!is.null(scenarios)) {
    diffScenarios <- setdiff(scenarios, uniqueScenarios)
    if (length(diffScenarios) > 0) {
      stop(paste0("Missing scenarios: ", paste0(setdiff(scenarios, uniqueScenarios), collapse = ", "), "\n"))
    }
    theseScenarios <- scenarios
  } else {
    theseScenarios <- uniqueScenarios
  }

  if (!is.null(variables)) {
    diffVariables <- setdiff(variables, uniqueVariables)
    if (length(diffVariables) > 0) {
      stop(paste0("Missing variables: ", paste0(diffVariables, collapse = ", "), "\n"))
    }
    theseVariables <- variables
  } else {
    theseVariables <- uniqueVariables
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
