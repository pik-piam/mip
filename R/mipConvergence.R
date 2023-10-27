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
#' @importFrom dplyr summarise group_by mutate filter
#' @importFrom quitte as.quitte
#' @importFrom plotly ggplotly config hide_legend subplot layout
#' @importFrom reshape2 dcast
#'
#' @export
mipConvergence <- function(gdx) {

  if (!file.exists(gdx)) {
    warning("gdx file not found!")
    return(list())
  }

  modelstat <- readGDX(gdx, name = "o_modelstat")[[1]]
  lastIteration <- readGDX(gdx, name = "o_iterationNumber")[[1]]

  if (!(modelstat %in% c(1, 2, 3, 4, 5, 6, 7))) {
    warning("Run failed - Check code, pre-triangular infes ...")
    return(list())
  }

  aestethics <- list(
    "alpha" = 0.6,
    "line" = list("size" = 2 / 3.78),
    "point" = list("size" = 2 / 3.78)
  )

  booleanColor <- c("yes" = "#00BFC4", "no" = "#F8766D")

  # Optimality / Objective Deviation ----

  p80ConvNashObjValIter <- readGDX(gdx, name = "p80_convNashObjVal_iter") %>%
    as.quitte() %>%
    select(c("region", "iteration", "objvalDifference" = "value")) %>%
    mutate(iteration := as.numeric(iteration)) %>%
    filter(iteration <= lastIteration)

  p80RepyIteration <- readGDX(gdx, name = "p80_repy_iteration", restore_zeros = FALSE) %>%
    as.quitte() %>%
    select(c("solveinfo80", "region", "iteration", "value")) %>%
    mutate(iteration := as.numeric(iteration)) %>%
    dcast(region + iteration ~ solveinfo80, value.var = "value")

  p80RepyIteration <- p80RepyIteration %>%
    left_join(p80ConvNashObjValIter, by = c("region", "iteration")) %>%
    group_by(.data$region) %>%
    mutate(
      objvalCondition = ifelse(modelstat == "2", TRUE,
        ifelse(modelstat == "7" & is.na(.data$objvalDifference), FALSE,
          ifelse(modelstat == "7" & .data$objvalDifference < -1e-4, FALSE, TRUE)
        )
      )
    ) %>%
    ungroup() %>%
    group_by(.data$iteration) %>%
    mutate(objvalConverge = all(.data$objvalCondition)) %>%
    ungroup()

  data <- p80RepyIteration %>%
    select("iteration", "objvalConverge") %>%
    distinct() %>%
    mutate(
      !!sym("objVarCondition") := ifelse(.data$objvalConverge, "yes", "no"),
      tooltip := paste0("Iteration: ", .data$iteration, "<br>Converged")
    )

  for (iter in unique(data$iteration)) {
    current <- filter(p80RepyIteration, .data$iteration == iter)

    if (!all(current$objvalCondition)) {
      tooltip <- NULL
      current <- filter(current, .data$objvalCondition == FALSE)

      for (reg in current$region) {
        diff <- current[current$region == reg, ]$objvalDifference
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


  # Feasibility -----

  p80RepyIteration <- readGDX(gdx, name = "p80_repy_iteration", restore_zeros = FALSE) %>%
    as.quitte() %>%
    select(c("solveinfo80", "region", "iteration", "value")) %>%
    dcast(region + iteration ~ solveinfo80, value.var = "value") %>%
    mutate(
      iteration := as.numeric(iteration),
      convergence := case_when(
        modelstat == 1 & solvestat == 1 ~ "optimal",
        modelstat == 2 & solvestat == 1 ~ "optimal",
        modelstat == 7 & solvestat == 4 ~ "feasible",
        .default = "infeasible"
      )
    )

  data <- p80RepyIteration %>%
    group_by(.data$iteration, .data$convergence) %>%
    mutate(details = paste0("Iteration: ", .data$iteration, "<br>region: ", paste0(.data$region, collapse = ", "))) %>%
    ungroup()

  data$convergence <- factor(data$convergence, levels = c("infeasible", "feasible", "optimal"))

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
    scale_fill_manual(values = c("optimal" = "#00BFC4", "feasible" = "#ffcc66", "infeasible" = "#F8766D")) +
    scale_color_manual(values = plotstyle(as.character(unique(data$region)))) +
    scale_y_discrete(breaks = c("infeasible", "feasible", "optimal"), drop = FALSE) +
    theme_minimal() +
    labs(x = NULL, y = NULL)

  convergencePlotPlotly <- ggplotly(convergencePlot, tooltip = c("text"))

  # Trade goods surplus detail ----

  # TODO: why is p80_surplusMax_iter only returning positive values?
  surplus <- readGDX(gdx, name = "p80_surplus", restore_zeros = FALSE)[, c(2100, 2150), ] %>%
    as.quitte() %>%
    select(c("period", "value", "all_enty", "iteration")) %>%
    mutate(
      value := ifelse(is.na(value), 0, value),
      type := case_when(
        all_enty == "good" ~ "Goods trade surplus",
        all_enty == "perm" ~ "Permits",
        TRUE ~ "Primary energy trade surplus"
      )
    )

  p80SurplusMaxTolerance <- readGDX(gdx, name = "p80_surplusMaxTolerance", restore_zeros = FALSE) %>%
    as.quitte() %>%
    select(c("maxTol" = 7, "all_enty" = 8))

  surplus <- left_join(surplus, p80SurplusMaxTolerance, by = "all_enty") %>%
    mutate(
      maxTol := ifelse(period == 2150, maxTol * 10, maxTol),
      withinLimits := ifelse(abs(value) > maxTol, "no", "yes")
    )

  data <- surplus

  data$tooltip <- paste0(
    ifelse(data$withinLimits == "no",
      ifelse(data$value > data$maxTol,
        paste0(
          data$all_enty, " trade surplus (", data$value,
          ") is greater than maximum tolerance (", data$maxTol, ")."
        ),
        paste0(
          data$all_enty, " trade surplus (", data$value,
          ") is lower than maximum tolerance (-", data$maxTol, ")."
        )
      ),
      paste0(data$all_enty, " is within tolerance.")
    ),
    "<br>Iteration: ", data$iteration
  )

  limits <- surplus %>%
    group_by(.data$type, .data$period, .data$iteration) %>%
    mutate(withinLimits = ifelse(all(.data$withinLimits == "yes"), "yes", "no")) %>%
    ungroup() %>%
    select("type", "period", "iteration", "maxTol", "withinLimits") %>%
    distinct() %>%
    mutate(
      rectXmin = as.numeric(iteration) - 0.5,
      rectXmax = as.numeric(iteration) + 0.5,
      tooltip = paste0(
        type,
        ifelse(withinLimits == "no",
          " outside tolerance limits.",
          " within tolerance limits."
        )
      )
    )

  surplusColor <- c(
    peoil = "#cc7500",
    pegas = "#999959",
    pecoal = "#0c0c0c",
    peur = "#EF7676",
    pebiolc = "#005900",
    good = "#00BFC4"
  )

  surplusConvergence <- ggplot() +
    suppressWarnings(geom_line(
      data = data,
      aes_(
        x = ~iteration, y = ~value, color = ~all_enty,
        group = ~all_enty, text = ~tooltip
      ),
      alpha = aestethics$alpha,
      size = aestethics$line$size
    )) +
    suppressWarnings(geom_rect(
      data = limits,
      aes_(
        xmin = ~rectXmin, xmax = ~rectXmax,
        ymin = ~ -maxTol, ymax = ~maxTol,
        fill = ~withinLimits, text = ~tooltip
      ),
      inherit.aes = FALSE,
      alpha = aestethics$alpha
    )) +
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
    summarise(withinLimits = ifelse(all(.data$withinLimits == "yes"), "yes", "no")) %>%
    mutate(tooltip = paste0("Iteration: ", iteration, "<br>Converged"))

  for (iter in surplusCondition$iteration) {
    if (surplusCondition[which(surplusCondition$iteration == iter), ]$withinLimits == "no") {
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


  # Price anticipation ----

  cmMaxFadeoutPriceAnticip <- as.vector(readGDX(gdx, name = "cm_maxFadeoutPriceAnticip"))
  p80FadeoutPriceAnticipIter <- readGDX(gdx, name = "p80_fadeoutPriceAnticip_iter", restore_zeros = FALSE) %>%
    as.quitte() %>%
    select("iteration", "fadeoutPriceAnticip" = value )

  data <- p80FadeoutPriceAnticipIter %>%
    mutate(
      iteration := as.numeric(iteration),
      converged = ifelse(.data$fadeoutPriceAnticip > cmMaxFadeoutPriceAnticip, "no", "yes"),
      tooltip = ifelse(
        .data$converged == "yes",
        paste0(
          "Converged<br>Price Anticipation fade out is low enough<br>",
          round(.data$fadeoutPriceAnticip, 5), " <= ", cmMaxFadeoutPriceAnticip
        ),
        paste0(
          "Not converged<br>Price Anticipation fade out is not low enough<br>",
          round(.data$fadeoutPriceAnticip, 5), " > ", cmMaxFadeoutPriceAnticip
        )
      )
    )

  priceAnticipation <- ggplot(data, aes_(x = ~iteration)) +
    geom_line(aes_(y = ~fadeoutPriceAnticip), alpha = 0.3, size = aestethics$line$size) +
    suppressWarnings(geom_point(
      size = 2,
      aes_(y = 0.0001, fill = ~converged, text = ~tooltip),
      alpha = aestethics$alpha
    )) +
    theme_minimal() +
    scale_fill_manual(values = booleanColor) +
    scale_y_continuous(breaks = c(0.0001), labels = c("Price\nAnticipation")) +
    scale_x_continuous(breaks = c(data$iteration)) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(ylim = c(-0.2, 1))

  priceAnticipationPlotly <- ggplotly(priceAnticipation, tooltip = c("text"))

  # Tax Convergence ----

  # Summary plot ----

  out <- list()

  out$plot <- subplot(
    convergencePlotPlotly,
    objVarSummaryPlotly,
    surplusSummaryPlotly,
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
