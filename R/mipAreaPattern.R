#' Generic area plot function. Automatically creates facet grid from data. Optionally adds total line. Supports patterns.
#'
#' @param x Data to plot. Allowed data formats: magpie or quitte. NOTE: To ensure correct conversion to quitte objects,
#' the dimension that contains the variables must have one of the following names: variable, scenario, or model.
#' @param stack_priority Name of column you want to stack. If you provide more than one column name the
#' function will scan the columns in the given order and use the first dimension for stacking that has
#' more than one element.
#' @param total total data to plot. Allowed inputs: magpie, quitte or boolean. If total data is
#' provided by user in magpie or quitte format it will be added to the plot. If user sets total to
#' TRUE total will be calculated by the function and added to the plot. If total is FALSE the plot
#' will ignore it.
#' @param scales scales can be fixed ("fixed", default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param shorten Shorten variable names (default is TRUE) by removing categories only if they are identical (for short
#' names in the legend)
#' @param hist Historical data. Allowed data formats: magpie or quitte. NOTE: To ensure correct conversion to quitte
#' objects, the dimension that contains the variables must have one of the following names: variable, scenario, model.
#' @param hist_source If there are multiple historical sources the name of the source that you want to be plotted.
#' @param patternDensity Sets globally the density of all patterns
#' @param patternKeyScale Sets globally the size of all patterns in the legend items
#' @author David Klein, Jan Philipp Dietrich, Paul Effing
#' @section Example Plot:
#' \if{html}{\figure{mipAreaPattern.png}{example plot}}
#' @examples
#' p <- mipAreaPattern(x = mip_example_data)
#' # create plot with best-guess design (internally using theme_mip(size=12))
#' p <- mipAreaPattern(mip_example_data)
#' # override default theme with theme_grey and move legend to top
#' library(ggplot2)
#' p <- p + theme_grey() + theme(legend.position = "top")
#' # go back to theme_mip and increase font size
#' p <- p + theme_mip(size = 18)
#' # change facetting
#' p <- p + facet_grid(region ~ scenario)
#'
#' # patterns and color scales can be added manually here:
#' # p <- p + scale_pattern_manual(values = patternValues,
#'                      labels = labels) +
#' # p <- p + scale_pattern_spacing_manual(values= spacingValues,
#'                                labels = labels)+
#' # p <- p + scale_fill_manual(values=colorFillValues,
#'                     labels = labels)
#' @importFrom magclass is.magpie getSets
#' @importFrom quitte is.quitte
#' @importFrom ggplot2 ggplot aes_ geom_line scale_linetype_discrete
#'   facet_wrap facet_grid theme scale_fill_manual xlab expand_limits
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom rlang .data
#' @importFrom ggpattern geom_area_pattern scale_pattern_manual scale_pattern_spacing_manual
#' @export
mipAreaPattern <- function(x, stack_priority = c("variable", "region"), total = TRUE, scales = "fixed", shorten = TRUE, #nolint
                    hist = NULL, hist_source = "first", patternDensity = 0.35, patternKeyScale =0.6) { #nolint
  ############################################
  ######  P R E P A R E   D A T A  ###########
  ############################################

  # To ensure correct conversion to quitte objects, there should be at least
  # one dimension named "variable" or "scenario" or "model" in MAgPIE objects.
  if (is.magpie(x) & !any(c("variable", "model", "scenario") %in% getSets(x))) {
    stop("MAgPIE objects need to have at least one dimension named 'variable' or 'model' or 'scenario'.")
  }

  x <- as.quitte(x)

  # Add dummy scenario if data does not contain a scenario
  # Otherwise selecting all scenarios that are not 'historicl' further down deletes ALL data
  if (all(is.na(x$scenario))) x$scenario <- "default"

  # shorten variable names and calc ylab
  if (shorten) x$variable <- shorten_legend(x$variable, identical_only = TRUE)
  ylab <- paste0(sub(".$", "", attr(x$variable, "front")), attr(x$variable, "back"))
  # add unit
  unit <- unique(as.character(x$unit))
  ylab <- paste0(ylab, " (", paste0(unit, collapse = " | "), ")")

  # Repeat the same for history
  if (!is.null(hist)) {
    if (is.magpie(hist) & !any(c("variable", "model", "scenario") %in% getSets(hist))) {
      stop("MAgPIE objects with historical data need to have at least one dimension named 'variable' or 'model' or ",
           "'scenario'.")
    }
    hist <- as.quitte(hist)
    if (shorten) hist$variable <- shorten_legend(hist$variable, identical_only = TRUE)
    # select historical data source
    if (hist_source == "first") {
      hist <- hist[hist$model == levels(hist$model)[1], ]
    } else if (hist_source %in% levels(hist$model)) {
      hist <- hist[hist$model == hist_source, ]
    } else {
      warning(paste0(hist_source, " could not be found in the historical data!"))
      hist <- NULL
    }
  }

  ###
  ### Find out which variables to stack and which to put to facet_grid
  ###

  # count levels of the given columns
  nLevels <- vapply(subset(x, select = c("variable", "region", "scenario", "model")), nlevels, integer(1))

  # Find first dimension in stack_priority that has more than one element.
  # Initialize dimToStack (this applies if if all dimensions have only one element)
  dimToStack <- stack_priority[1]
  for (s in stack_priority) {
    if (nLevels[s] > 1) {
      dimToStack <- s
      break
    }
  }

  # find and sort dimension with more than one element, exclude dimToStack
  # sort: to be able to build the interaction with the smallest number of resulting combinations (further down)
  facets <- sort(nLevels[nLevels > 1])
  facets <- setdiff(names(facets), dimToStack)

  # Combine data and historical data into one object
  if (!is.null(hist)) x <- rbind(x, hist)

  # if there are three facet dimensions that have more than one element combine the two
  # smallest ones into the first one to be able to create a 2-D facet_grid later on
  if (length(facets) == 3) {
    x[, facets[1]] <- interaction(x[[facets[1]]], x[[facets[2]]])
  }

  # if not provided by user calculate total by summing over dimToStack
  if (isTRUE(total)) {
    dimToGroup <- setdiff(c("model", "scenario", "region", "variable", "unit", "period"), dimToStack)
    total <- x %>%
      group_by(.data[[dimToGroup[1]]], .data[[dimToGroup[2]]], .data[[dimToGroup[3]]],
               .data[[dimToGroup[4]]], .data[[dimToGroup[5]]]) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()
    # add missing column dimToStack to make it convertable to quitte
    total[, dimToStack] <- "Total"
  }

  # convert total to quitte
  if (!identical(FALSE, total)) total <- as.quitte(total)

  # separate positive and negative parts of data for area plot
  tmp <- droplevels(x[x$scenario != "historical", ])

  pos <- tmp
  neg <- tmp

  pos$value <- ifelse(tmp$value >= 0, tmp$value, 0)
  neg$value <- ifelse(tmp$value < 0, tmp$value, -1e-36)

  if (!is.null(hist)) {
    tmp <- droplevels(x[x$scenario == "historical", ])
    postmp <- tmp
    negtmp <- tmp

    postmp$value <- ifelse(tmp$value >= 0, tmp$value, 0)
    negtmp$value <- ifelse(tmp$value < 0, tmp$value, -1e-36)

    posH <- NULL
    negH <- NULL

    # repeat historical data as often as there are scenarios
    # so that they will be plotted for each single scenario
    for (l in levels(pos$scenario)) {
      postmp$scenario <- factor(l)
      negtmp$scenario <- factor(l)

      posH <- rbind(posH, postmp)
      negH <- rbind(negH, negtmp)
    }
  }

  # split historical and model total
  if (!identical(FALSE, total)) {
    totalX <- droplevels(total[total$scenario != "historical", ])
  }

  if (!is.null(hist) & !identical(FALSE, total)) {
    tottmp <- droplevels(total[total$scenario == "historical", ])
    # repeat historical total as often as there are scenarios
    # so that it will be plotted for each single scenario
    totalH <- NULL
    for (l in levels(pos$scenario)) {
      tottmp$scenario <- factor(l)
      totalH <- rbind(totalH, tottmp)
    }
  }

  ############################################
  ###############  P L O T  ##################
  ############################################

  p <- ggplot() +
    geom_area_pattern(data = pos, aes_(~period, ~value, pattern = as.formula(paste("~", dimToStack)), fill = as.formula(paste("~", dimToStack)), pattern_spacing = as.formula(paste("~", dimToStack))), pattern_density = patternDensity, pattern_key_scale_factor = patternKeyScale) +
    geom_area_pattern(data = neg, aes_(~period, ~value, pattern = as.formula(paste("~", dimToStack)), fill = as.formula(paste("~", dimToStack)), pattern_spacing = as.formula(paste("~", dimToStack))), pattern_density = patternDensity, pattern_key_scale_factor = patternKeyScale)

  if (!is.null(hist)) {
    p <- p + geom_area_pattern(data = posH, aes_(~period, ~value, pattern = as.formula(paste("~", dimToStack)), fill = as.formula(paste("~", dimToStack)), pattern_spacing = as.formula(paste("~", dimToStack))), pattern_density = patternDensity, pattern_key_scale_factor = patternKeyScale, alpha = 0.3)
    p <- p + geom_area_pattern(data = negH, aes_(~period, ~value, pattern = as.formula(paste("~", dimToStack)), fill = as.formula(paste("~", dimToStack)), pattern_spacing = as.formula(paste("~", dimToStack))), pattern_density = patternDensity, pattern_key_scale_factor = patternKeyScale, alpha = 0.3)
  }


  # define facet_grid
  if (length(facets) == 1) p <- p + facet_wrap(as.formula(paste("~", facets)), scales = scales)
  if (length(facets) == 2) p <- p + facet_grid(as.formula(paste(facets[1], "~", facets[2])), scales = scales)
  # facet 1 and 2 are combined in dim 1
  if (length(facets) == 3) p <- p + facet_grid(as.formula(paste(facets[1], "~", facets[3])), scales = scales)

  # add total to plot as black line
  if (is.quitte(total)) {
    p <- p + geom_line(data = totalX, aes_(~period, ~value, linetype = as.formula(paste("~", dimToStack))),
                       color = "#000000", size = 1)
    p <- p + scale_linetype_discrete(labels = "Total", name = "")
    if (!is.null(hist)) {
      p <- p + geom_line(data = totalH, aes_(~period, ~value, linetype = as.formula(paste("~", dimToStack))),
                         color = "#000000", size = 1, alpha = 0.3)
    }
  }

  # plot settings
  p <- p + xlab("Year")
  p <- p + ylab(ylab)

  # update theme
  p <- p + theme_minimal()
  p <- p + theme_mip(size = 12)
  p <- p + theme(axis.title.x = element_blank())

  # If data that is to be stacked are not factors, convert it to factors.
  # This is useful since we can use the same call of plotstyle as in other mip functions and thus
  # get the same order of colors for elements that are not defined in plotstyle.
  if (!is.factor(x[[dimToStack]])) x[[dimToStack]] <- factor(x[[dimToStack]], levels = unique(x[[dimToStack]]))
  # because of conflicts with patterns, dont use plotstyle colours and labels by default
  p <- p + scale_pattern_manual(values= rep("none", length(unique(x[[dimToStack]])))) +
           scale_pattern_spacing_manual(values= rep(0.0, length(unique(x[[dimToStack]]))))

  # increase y-axis limits to hide all-zero data that was set to -1e-36
  p <- p +
    expand_limits(y = c(-1, 1) * 1e-32)

  return(p)
}
