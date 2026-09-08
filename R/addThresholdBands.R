#' Add Threshold Bands to a Line Plot
#'
#' Draws colored background bands for validation thresholds (as computed by
#' \code{piamValidation::getThresholdBands()}) below the layers of an existing
#' line plot. Bands are only drawn if the plot shows exactly one variable and
#' only where unit, region and period match the plotted data. Thresholds of
#' contiguous periods are drawn as ribbons, isolated periods as vertical bars.
#'
#' @param p ggplot object faceted by region, e.g. created by
#'   \code{mipLineHistorical()}.
#' @param plotData quitte-style data.frame with the data shown in \code{p},
#'   used to restrict the bands to the plotted variable, unit, regions and
#'   periods.
#' @param thresholds data.frame with the columns \code{variable}, \code{unit},
#'   \code{region}, \code{period}, \code{min_red}, \code{min_yel},
#'   \code{max_yel} and \code{max_red}.
#' @return \code{p} with threshold bands added below its other layers.
#' @importFrom dplyr arrange filter group_by lag mutate n ungroup
#' @importFrom ggplot2 aes geom_ribbon geom_linerange
addThresholdBands <- function(p, plotData, thresholds) {

  # bands are only well-defined if the plot shows exactly one variable
  vars <- unique(as.character(plotData$variable))
  if (length(vars) != 1) {
    return(p)
  }

  bands <- thresholds %>%
    filter(
      as.character(.data$variable) %in% vars,
      as.character(.data$unit) %in% unique(as.character(plotData$unit)),
      as.character(.data$region) %in% unique(as.character(plotData$region)),
      .data$period >= min(plotData$period),
      .data$period <= max(plotData$period))
  if (nrow(bands) == 0) {
    return(p)
  }

  # align region factor levels with the plotted data so facets match
  regionLevels <- if (is.factor(plotData$region)) {
    levels(plotData$region)
  } else {
    unique(as.character(plotData$region))
  }
  bands$region <- factor(as.character(bands$region), levels = regionLevels)

  # colors and transparencies as in piamValidation::linePlotThresholds()
  layers <- c(
    thresholdBandLayers(bands, "min_yel", "max_yel", "#008450", 0.20), # green
    thresholdBandLayers(bands, "max_yel", "max_red", "#EFB700", 0.20), # yellow
    thresholdBandLayers(bands, "min_red", "min_yel", "#66ccee", 0.30)) # blue

  # insert the bands below all existing layers
  p$layers <- c(layers, p$layers)
  return(p)
}


# Creates the ggplot layers for one threshold band, i.e. the area between the
# thresholds `lower` and `upper`. Per region, contiguous periods where both
# thresholds are defined become a ribbon, isolated periods a vertical bar.
thresholdBandLayers <- function(bands, lower, upper, fill, alpha) {

  d <- bands %>%
    arrange(.data$region, .data$period) %>%
    group_by(.data$region) %>%
    mutate(
      valid = !is.na(.data[[lower]]) & !is.na(.data[[upper]]),
      bandGroup = paste(
        .data$region,
        cumsum(.data$valid != lag(.data$valid, default = .data$valid[1])))) %>%
    ungroup() %>%
    filter(.data$valid) %>%
    group_by(.data$bandGroup) %>%
    mutate(nPeriods = n()) %>%
    ungroup()

  layers <- list()

  # ribbons for thresholds spannend multiple periods
  dMulti <- filter(d, .data$nPeriods > 1)
  if (nrow(dMulti) > 0) {
    layers <- c(layers, list(
      geom_ribbon(
        data = dMulti,
        aes(x = .data$period, ymin = .data[[lower]], ymax = .data[[upper]],
            group = .data$bandGroup),
        fill = fill, alpha = alpha, color = NA, inherit.aes = FALSE)))
  }

  # bars for single year thresholds
  dSingle <- filter(d, .data$nPeriods == 1)
  if (nrow(dSingle) > 0) {
    layers <- c(layers, list(
      geom_linerange(
        data = dSingle,
        aes(x = .data$period, ymin = .data[[lower]], ymax = .data[[upper]]),
        linewidth = 6, color = fill, alpha = alpha, inherit.aes = FALSE)))
  }

  return(layers)
}
