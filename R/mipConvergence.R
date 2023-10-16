#' @title Create REMIND convergence overview
#'
#' @param gdx GDX file
#' @author Renato Rodrigues, Falk Benke
#'
#' @examples
#'
#'   \dontrun{
#'     mipConvergence(gdx="fulldata.gdx")
#'   }
#'
#' @importFrom gdx readGDX
#' @importFrom dplyr bind_rows summarise group_by mutate filter
#' @importFrom quitte as.quitte
#' @importFrom ggplot2 ggplot geom_point geom_line scale_fill_manual
#'   scale_y_discrete geom_rect geom_hline scale_x_continuous
#'   coord_cartesian aes_
#' @importFrom plotly ggplotly config hide_legend subplot layout
#' @importFrom reshape2 dcast
#' @importFrom stats lag
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
mipConvergence <- function(gdx) {

  if (!file.exists(gdx)) {
    stop("gdx file not found!")
  }

  modelstat <- readGDX(gdx, name = "o_modelstat")[[1]]

  if (!(modelstat %in% c(1, 2, 3, 4, 5, 6, 7))) {
    stop("Run failed - Check code, pre-triangular infes ...")
  }

  aestethics <- list(
    "alpha" = 0.6,
    "line" = list("size" = 2 / 3.78),
    "point" = list("size" = 2 / 3.78)
  )

  missingColors <- c(
    "DEU" = "#7F2704",
    "EUW" = "#FC4E2A", "EWN" = "#FC4E2A", "FRA" = "#E31A1C",
    "EUS" = "#FFEDA0", "ESW" = "#FFEDA0", "ESC" = brewer.pal(9, "YlOrRd")[3],
    "EUC" = "#969696", "ECS" = "#D9D9D9", "ECE" = "#969696",
    "EUN" = "#4292C6", "ENC" = "#6BAED6", "UKI" = "#4292C6",
    "NEU" = "#78C679", "NEN" = "#78C679", "NES" = "#D9F0A3",
    "CHE" = "#78C679", "ENN" = "#78C679", "ESE" = "#D9F0A3", "EUI" = "#78C679", "ROE" = "#D9F0A3", # older EU
    "SSA" = "#00BAFF", "REF" = "#D900BC", "CAZ" = "#007362", "CHA" = "#F24200",
    "Uranium" = "#EF7676", "Goods" = "#00BFC4",
    "optimal" = "#00BFC4", "feasible" = "#ffcc66", "infeasible" = "#F8766D",
    "yes" = "#00BFC4", "no" = "#F8766D",
    "optimal_alt" = "#00BFC4", "feasible_alt" = "#ffcc66"
  )

  missingColorsdf <- data.frame(row.names = names(missingColors), color = missingColors)

  # data preparation ----

  p80_repy_wide <- readGDX(gdx, name = "p80_repy_iteration", restore_zeros = FALSE) %>%
    as.quitte() %>%
    select(c("solveinfo80", "region", "iteration", "value")) %>%
    dcast(region + iteration ~ solveinfo80, value.var = "value")

  p80_repy_wide <- p80_repy_wide %>%
    group_by(.data$region) %>%
    mutate(
      diff.objval = .data$objval - lag(.data$objval, order_by = .data$iteration),
      objvalCondition = ifelse(modelstat == "2", TRUE,
        ifelse(modelstat == "7" & is.na(.data$diff.objval), FALSE,
          ifelse(modelstat == "7" & abs(.data$diff.objval) < 1e-4, TRUE, FALSE)
        )
      )
    ) %>%
    ungroup()

  p80_repy_wide <- p80_repy_wide %>%
    group_by(.data$iteration) %>%
    mutate(objvalConverge = all(.data$objvalCondition))

  p80_repy_wide$convergence <- "infeasible"
  p80_repy_wide[(p80_repy_wide$modelstat == 1 & p80_repy_wide$solvestat == 1), "convergence"] <- "optimal"
  p80_repy_wide[(p80_repy_wide$modelstat == 2 & p80_repy_wide$solvestat == 1), "convergence"] <- "optimal"
  p80_repy_wide[(p80_repy_wide$modelstat == 7 & p80_repy_wide$solvestat == 4), "convergence"] <- "feasible"

  data <- p80_repy_wide %>%
    group_by(.data$iteration, .data$convergence) %>%
    mutate(details = paste0("Iteration: ", .data$iteration, "<br>region: ", paste0(.data$region, collapse = ", "))) %>%
    ungroup()

  data$convergence <- factor(data$convergence, levels = c("infeasible", "feasible", "optimal"))

  # Convergence plot -----

  convergencePlot <-
    suppressWarnings(ggplot(mapping = aes_(~iteration, ~convergence, text = ~details))) +
    geom_line(
      data = data,
      linetype = "dashed",
      aes_(group = ~region, color = ~region),
      alpha = aestethics$alpha,
      linewidth = aestethics$line$size
    ) +
    geom_point(
      data = select(data, c("iteration", "convergence", "details")) %>% distinct(),
      aes_(fill = ~convergence),
      size = 2,
      alpha = aestethics$alpha
    ) +
    scale_fill_manual(values = plotstyle(as.character(unique(data$convergence)), unknown = missingColorsdf)) +
    scale_color_manual(values = plotstyle(as.character(unique(data$region)), unknown = missingColorsdf)) +
    scale_y_discrete(breaks = c("infeasible", "feasible", "optimal"), drop = FALSE) +
    theme_minimal() +
    labs(x = NULL, y = NULL)


  convergencePlotPlotly <- ggplotly(convergencePlot, tooltip = c("text"))

  # Trade goods surplus detail ----

  surplus <- readGDX(gdx, name = "p80_surplus", restore_zeros = FALSE)[, c(2100, 2150), ] %>%
    as.quitte() %>%
    select(c("period", "value", "all_enty", "iteration")) %>%
    mutate(value := ifelse(is.na(value), 0, value),
           type := case_when(
             all_enty == "good" ~ "Goods trade surplus",
             all_enty == "perm" ~ "Permits",
             TRUE ~ "Primary energy trade surplus"
           ))

  maxTol <- readGDX(gdx, name = "p80_surplusMaxTolerance", restore_zeros = FALSE) %>%
    as.quitte() %>%
    select(c("maxTol" = 7, "all_enty" = 8))

  surplus <- merge(surplus, maxTol, by = "all_enty")
  surplus[which(surplus$period == 2150), ]$maxTol <- surplus[which(surplus$period == 2150), ]$maxTol * 10
  surplus$rectXmin <- as.numeric(surplus$iteration) - 0.5
  surplus$rectXmax <- as.numeric(surplus$iteration) + 0.5
  surplus$withinLimits <- ifelse(surplus$value > surplus$maxTol, "no",
                                 ifelse(surplus$value < -surplus$maxTol, "no", "yes"))

  maxTol <- surplus %>%
    group_by(.data$type, .data$period, .data$iteration) %>%
    mutate(withinLimits = ifelse(all(.data$withinLimits == "yes"), "yes", "no")) %>%
    ungroup() %>%
    filter(.data$all_enty %in% c("peoil", "good", "perm")) %>%
    select(-1)

  vars <- c("pecoal" = "Coal",
            "pegas" = "Gas",
            "peoil" = "Oil",
            "peur" = "Uranium",
            "good" = "Goods",
            "pebiolc" = "Biomass")
  surplus$name <- vars[surplus$all_enty]

  booleanColor <- plotstyle(as.character(unique(maxTol$withinLimits)), unknown = missingColorsdf)
  surplusColor <- plotstyle(vars, unknown = missingColorsdf)
  names(surplusColor) <- names(vars)

  surplus$tooltip <- paste0(
    ifelse(surplus$withinLimits == "no",
      ifelse(surplus$value > surplus$maxTol,
        paste0(surplus$name, " trade surplus (", surplus$value,
               ") is greater than maximum tolerance (", surplus$maxTol, ")."),
        paste0(surplus$name, " trade surplus (", surplus$value,
               ") is lower than maximum tolerance (-", surplus$maxTol, ").")
      ),
      paste0(surplus$type, " is within tolerance.")
    ),
    "<br>Iteration: ", surplus$iteration
  )

  maxTol$tooltip <- paste0(maxTol$type,
                           ifelse(maxTol$withinLimits == "no",
                                  " outside tolerance limits.",
                                  " within tolerance limits."))

  surplusConvergence <- ggplot() +
    suppressWarnings(geom_line(data = surplus,
                               aes_(x = ~iteration, y = ~value, color = ~all_enty,
                                    group = ~all_enty, text = ~tooltip),
                               alpha = aestethics$alpha,
                               size = aestethics$line$size)) +
    suppressWarnings(geom_rect(data = maxTol,
                               aes_(xmin = ~rectXmin, xmax = ~rectXmax,
                                    ymin = ~ -maxTol, ymax = ~maxTol,
                                    fill = ~withinLimits, text = ~tooltip),
                               inherit.aes = FALSE,
                               alpha = aestethics$alpha)) +
    theme_minimal() +
    ggtitle("Tradable goods surplus") +
    facet_grid(type ~ period, scales = "free_y") +
    scale_color_manual(values = surplusColor) +
    scale_fill_manual(values = booleanColor) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  surplusConvergencePlotly <- ggplotly(surplusConvergence, tooltip = c("text")) %>%
    hide_legend() %>%
    config(displayModeBar = FALSE, displaylogo = FALSE)

  # Trade surplus summary ----

  surplusCondition <- surplus %>%
    group_by(.data$iteration) %>%
    summarise(withinLimits = ifelse(all(.data$withinLimits == "yes"), "yes", "no"))

  surplusCondition$tooltip <- paste0("Iteration: ", surplusCondition$iteration, "<br>Converged")

  for (iter in surplusCondition$iteration) {
    if (all(surplusCondition[which(surplusCondition$iteration == iter), ]$withinLimits == "no")) {
      tooltip <- NULL
      for (period in unique(surplus$period)) {
        for (good in unique(surplus$all_enty)) {
          currSurplus <- surplus[which(surplus$iteration == iter & surplus$period == period &
                                         surplus$all_enty == good), ]
          withinLimits <- ifelse(currSurplus$value > currSurplus$maxTol,
                                 "no", ifelse(currSurplus$value < -currSurplus$maxTol, "no", "yes"))
          if (withinLimits == "no") {
            tooltip <- paste0(tooltip, "<br> ", period, " | ", good, " | ",
                              ifelse(currSurplus$value > currSurplus$maxTol,
                                     paste0(round(currSurplus$value, 5), " > ", currSurplus$maxTol),
                                     paste0(round(currSurplus$value, 5), " < ", -currSurplus$maxTol)))
          }
        }
      }
      tooltip <- paste0(
        "Iteration: ", iter, "<br>Not converged",
        "<br>Period | Trade | Surplus", tooltip
      )
      surplusCondition[which(surplusCondition$iteration == iter), ]$tooltip <- tooltip
    }
  }

  surplusSummary <- suppressWarnings(ggplot(surplusCondition,
                                            aes_(x = ~iteration, y = "Trade\nSurplus",
                                                 fill = ~withinLimits, text = ~tooltip))) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    geom_point(size = 2, alpha = aestethics$alpha) +
    scale_fill_manual(values = booleanColor) +
    scale_y_discrete(breaks = c("Trade\nSurplus"), drop = FALSE) +
    labs(x = NULL, y = NULL)

  surplusSummaryPlotly <- ggplotly(surplusSummary, tooltip = c("text"))

  # Objective derivation ----

  data <- p80_repy_wide %>%
    select("iteration", "objvalConverge") %>%
    distinct() %>%
    mutate(
      !!sym("objVarCondition") := ifelse(isTRUE(.data$objvalConverge), "yes", "no"),
      tooltip := paste0("Iteration: ", .data$iteration, "<br>Converged")
    )

  for (iter in unique(data$iteration)) {

    current <- filter(p80_repy_wide, .data$iteration == iter)

    if (!all(current$objvalCondition)) {
      tooltip <- NULL
      current <- filter(current, .data$objvalCondition == FALSE)

      for (reg in current$region) {
        diff <- current[current$region == reg, ]$diff.objval
        tooltip <- paste0(tooltip, "<br> ", reg, "   |     ", round(diff, 5))
      }
      tooltip <- paste0(
        "Iteration: ", iter, "<br>Not converged",
        "<br>Region | Deviation", tooltip, "<br>The deviation limit is +- 0.0001"
      )
      data[which(data$iteration == iter), ]$tooltip <- tooltip
    }
  }

  objVarSummary <- suppressWarnings(ggplot(data, aes_(
    x = ~iteration, y = "Objective\nDeviation",
    fill = ~objVarCondition, text = ~tooltip
  ))) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    geom_point(size = 2, alpha = aestethics$alpha) +
    scale_fill_manual(values = booleanColor) +
    scale_y_discrete(breaks = c("Objective\nDeviation"), drop = FALSE) +
    labs(x = NULL, y = NULL)

  objVarSummaryPlotly <- ggplotly(objVarSummary, tooltip = c("text"))

  # Price anticipation ----

  priceAntecipationFadeoutIteration <- as.vector(readGDX(gdx, name = "s80_fadeoutPriceAnticipStartingPeriod"))
  lastIteration <- readGDX(gdx, name = "o_iterationNumber")[[1]]
  data <- data.frame(iteration = 1:lastIteration)

  data <- data %>% mutate(
    fadeoutPriceAnticip = ifelse(
      .data$iteration < priceAntecipationFadeoutIteration, 1,
      0.7**(.data$iteration - priceAntecipationFadeoutIteration + 1)
    ),
    converged = ifelse(.data$fadeoutPriceAnticip > 1e-4, "no", "yes"),
    tooltip = ifelse(
      .data$converged == "yes",
      paste0("Converged<br>Price Anticipation fade out is low enough<br>",
             round(.data$fadeoutPriceAnticip, 5), " <= 0.0001"),
      paste0("Not converged<br>Price Anticipation fade out is not low enough<br>",
             round(.data$fadeoutPriceAnticip, 5), " > 0.0001")
    )
  )

  priceAnticipation <- ggplot(data, aes_(x = ~iteration)) +
    geom_line(aes_(y = ~fadeoutPriceAnticip), alpha = 0.3, size = aestethics$line$size) +
    suppressWarnings(geom_point(size = 2,
                                aes_(y = 0.0001, fill = ~converged, text = ~tooltip),
                                alpha = aestethics$alpha)) +
    theme_minimal() +
    scale_fill_manual(values = booleanColor) +
    scale_y_continuous(breaks = c(0.0001), labels = c("Price\nAnticipation")) +
    scale_x_continuous(breaks = c(data$iteration)) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(ylim = c(-0.2, 1))

  priceAnticipationPlotly <- ggplotly(priceAnticipation, tooltip = c("text"))

  # Summary plot ----

  out <- list()

  out$plot <- subplot(
    convergencePlotPlotly,
    surplusSummaryPlotly,
    objVarSummaryPlotly,
    priceAnticipationPlotly,
    nrows = 4, shareX = TRUE, titleX = FALSE,
    heights = c(0.4, 0.2, 0.2, 0.2),
    margin = c(.1, .1, .1, .0001)
  ) %>%
    hide_legend() %>%
    config(displayModeBar = FALSE, displaylogo = FALSE) %>%
    layout(margin = list(l = -100, r = 10))

  out$tradeDetailPlot <- surplusConvergencePlotly

  return(out)
}
