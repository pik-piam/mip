#' Comparison plots show user selected variables obtained from different scenario
#' runs

#' @author Tonn Rueter
#' @param df `quitte` style data frame containing all variables for each scenario
#' @param scenarios Character vector containing the names of the desired scenarios. If none is provided, all scenarios will be displayed
#' @param variables Character vector containing the names of the desired variables. If none is provided, all variables will be displayed
#' @importFrom dplyr filter mutate
#' @importFrom reshape2 melt
#' @importFrom stringr str_extract
#' @export
plotPercentiles <- function(df, scenarios=NULL, variables=NULL) {

  # In the quitte data frame all perenctiles are given as individual variables
  # Manipulate input data frame such that all percentiles of a given quantity
  # are transformed to individual columns. Variable names in the quitte data
  # frame follow the format "Any|Variable|5.0th Percentile". The regular
  # expressions below divide the variable name into the prefix and the
  # percentile specifier
  data <- df %>%
    mutate(
      "percentile" = stringr::str_extract(variable, "[^\\|]+?$"),
      "variable"   = gsub("\\|[^\\|]+$", "", variable)
    ) %>%
    pivot_wider(
      names_from = "percentile",
      values_from = "value"
    )

  # Check which scenarios/variabes are available
  unique_scenarios <- unique(data$scenario)
  unique_variables <- unique(data$variable)

  # Check which function parameters have been provided and default to unique
  # values from the data frame in case none have
  these_scenarios <- if (is.null(scenarios)) {
    unique_scenarios
  } else if (all_items_available(scenarios, unique_scenarios, warn=TRUE)) {
    scenarios
  } else {
    stop("Provided scenario is missing in data")
  }
  these_variables <- if (is.null(variables)) {
    unique_variables
  } else if (all_items_available(variables, unique_variables, warn=TRUE)) {
    variables
  } else {
    stop("Provided variable is missing in data")
  }

  # Set up the plot
  p <- ggplot()

  # Fill plot by filtering for the requested variables and scenarios
  for (this_variable in these_variables) {
    for (this_scenario in these_scenarios) {
      plot_data <- filter(data, variable == this_variable & scenario == this_scenario)
      p <- p +
        geom_line(
          data = plot_data, aes(x = period, y = get("50.0th Percentile"))
        ) +
        geom_ribbon(
          data = plot_data, aes(x = period, ymin = get("33.0th Percentile"), ymax = get("67.0th Percentile")),
          fill = "#68788a", alpha = 0.5
        ) +
        geom_ribbon(
          data = plot_data, aes(x = period, ymin = get("5.0th Percentile"), ymax = get("95.0th Percentile")),
          fill = "#68788a", alpha = 0.2
        )
    }
  }

  # Depending on the function parameters, plots need to be arranged
  if (length(these_scenarios) == 1) {
    # Plots all parameters for a given scenario. Y-axes need to be independent
    p <- p +
      facet_wrap(
        vars(variable),
        scales = "free_y",
        ncol = 1) +
      theme(
        axis.title.x = element_blank()) +
      ylab(
        unique(data$unit))
  } else if (length(these_variables) == 1) {
    # Plots a given parameter for all scenarios. Lock y-axes to improve comparison
    p <- p +
      facet_wrap(
        vars(scenario)) +
      theme(
        axis.title.x = element_blank()) +
      ylab(
        unique(data$unit))
  }
  else {
    # Using facet grid when multiple variables in multiple scenarios are compared
    p <- p +
      facet_grid(
        variable ~ scenario,
        scales = "free_y") +
      theme(
        axis.title.x = element_blank()) +
      ylab(
        unique(data$unit))
  }

  return(p)
}

all_items_available <- function(selection, available, warn = FALSE) {
  for (item in selection) {
    if (!(item %in% available)) {
      if (warn) warning(paste0("'", item, "' missing in available data"))
      return(FALSE)
    }
  }
  return(TRUE)
}
