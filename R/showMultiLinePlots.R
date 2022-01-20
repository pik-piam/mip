# Creates two plots (main region + others) with the values of vars over time.
# Different regions are shown in the same plot. Faceting is done by variable.
# The plots arranged and shown and NULL is returned invisibly.
showMultiLinePlots <- function(
  data, vars, scales = "free_y",
  mainReg = getOption("mip.mainReg")
) {
  
  # Validate function arguments.
  data <- validateData(data, removeSuperfluousCols=TRUE)
  stopifnot(is.character(vars))
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided("mainReg")
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  
  data %>%
    filter(variable %in% vars) ->
    d
  d %>%
    filter(region == mainReg, scenario != "historical") ->
    dMainScen
  d %>%
    filter(region == mainReg, scenario == "historical") ->
    dMainHist
  d %>%
    filter(region != mainReg, scenario != "historical") ->
    dRegiScen
  d %>%
    filter(region != mainReg, scenario == "historical") ->
    dRegiHist
  
  warnMissingVars(dMainScen, vars)
  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  label <- paste0("[", paste0(unique(d$unit), collapse = ","), "]")
  
  dMainScen %>%
    ggplot(aes(period, value)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dMainHist, aes(shape = model)) +
    geom_line(data = dMainHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    ylim(0, NA) +
    ylab(label) ->
    p1
  
  regions <- unique(dRegiScen$region)
  dRegiScen %>%
    ggplot(aes(period, value, color = region)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dRegiHist, aes(shape = model)) +
    geom_line(data = dRegiHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    ylim(0, NA) +
    ylab(label) ->
    p2
  
  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")
  
  return(invisible(NULL))
}
