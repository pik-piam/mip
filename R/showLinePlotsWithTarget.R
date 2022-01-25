#' Show Line Plots With Target
#'
#' Shows line plots of a variable with additional target data.
#'
#' Creates a line plot showing single line plot of vars over time. Additionally
#' target values given in variables of the form \code{<vars>|target|<sth>} are
#' shown. The plot is shown.
#' @param vars A character vector. Usually just a single string. The variables
#'   to be plotted.
#' @inheritParams showLinePlots
#' @return \code{NULL} is returned invisible.
#' @examples
#' \dontrun{
#' showLinePlotsWithTarget(data, "Emi|GHG")
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom stringr str_detect
showLinePlotsWithTarget <- function(
  data, vars, scales = "free_y"
) {

  # Validate function arguments.
  stopifnot(is.quitte(data))
  stopifnot(is.character(vars))
  stopifnot(is.character(scales) && length(scales) == 1)

  vars %>%
    paste0("|target|") %>%
    str_replace_all(fixed("|"), fixed("\\|")) %>%
    paste0(collapse = "|") ->
    targetPattern
  data %>%
    filter(str_detect(.data$variable, .env$targetPattern)) %>%
    droplevels() ->
    dTar
  data %>%
    filter(.data$variable %in% .env$vars, .data$region %in% levels(.env$dTar$region)) %>%
    droplevels() ->
    d

  warnMissingVars(d, vars)
  if (NROW(d) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  label <- paste0(vars, " [", paste0(levels(d$unit), collapse = ","), "]")

  d %>%
    filter(.data$scenario != "historical") %>%
    droplevels() %>%
    mipLineHistorical(
      x_hist = d %>% filter(.data$scenario == "historical") %>% droplevels(),
      ylab = label,
      scales = scales,
      plot.priority = c("x_hist", "x", "x_proj"),
      facet.ncol = 3
    ) +
    geom_hline(
      data = dTar,
      aes(yintercept = .data$value),
      linetype = 2,
      color = "coral"
    ) +
    geom_vline(
      data = dTar,
      aes(xintercept = .data$period),
      linetype = 2,
      color = "coral"
    ) +
    geom_text(data = dTar, aes(
      x = max(.env$d$period) - (max(.env$d$period) - min(.env$d$period)) / 4,
      y = .data$value,
      label = paste(.data$variable, .data$period)
    )) ->
    p

  # Show plot.
  print(p)
  cat("\n\n")

  return(invisible(NULL))
}
