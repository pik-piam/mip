# Same as showMultiLinePlots() but with GDP on x-axis. When plotting by GDP,
# data from historical.mif is only shown for years where historical GDP is
# available. The plots arranged and shown and NULL is returned invisibly.
showMultiLinePlotsByVariable <- function(
  data, vars, xVar, scales = "free_y",
  mainReg = getOption("mip.mainReg"),
  histRefModel = getOption("mip.histRefModel")
) {
  
  # Validate function arguments.
  data <- validateData(data, removeSuperfluousCols=TRUE)
  stopifnot(is.character(vars))
  stopifnot(is.character(xVar) && length(xVar) == 1)
  stopifnot(is.character(scales) && length(scales) == 1)
  checkGlobalOptionsProvided(c("mainReg", "histRefModel"))
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  stopifnot(is.character(histRefModel) && !is.null(names(histRefModel)))
  stopifnot(xVar %in% names(histRefModel))
  
  data %>%
    filter(variable %in% vars) ->
    dy
  data %>%
    filter(variable %in% xVar) %>% 
    filter(scenario != "historical" | model == histRefModel[xVar]) ->
    dx
  dy %>% 
    left_join(dx, by=c("scenario", "region", "period"), suffix=c("", ".x")) %>% 
    drop_na() %>% 
    arrange(period) ->
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
  xLabel <- paste0(xVar, " [", paste0(unique(d$unit.x), collapse = ","), "]")
  
  dMainScen %>%
    ggplot(aes(value.x, value)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dMainHist, aes(shape = model)) +
    geom_line(data = dMainHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    ylim(0, NA) +
    ylab(label) + xlab(xLabel) ->
    p1
  
  dRegiScen %>%
    ggplot(aes(value.x, value, color = region)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dRegiHist, aes(shape = model)) +
    geom_line(data = dRegiHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    ylim(0, NA) +
    ylab(label) + xlab(xLabel) ->
    p2
  
  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")
  
  return(invisible(NULL))
}
