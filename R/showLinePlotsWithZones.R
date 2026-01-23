#' Show Line Plots With Zones
#'
#' Shows line plots of variables with additional (planetary boundary) zone overlay.
#'
#' Creates line plots using \code{createLinePlots()} and overlays colored
#' background zones indicating safe, intermediate, and high-risk ranges
#' based on given planetary boundary thresholds. Horizontal reference lines
#' mark the safe boundary and high-risk threshold. The zones can be applied
#' to the main plot, regional plots, or both.
#'
#' *RoSa*: Zones will be the same for all regions. Once regionalized PBs are
#' available, this functionality has to be added.
#'
#' @inheritDotParams createLinePlots
#' @param safePB Numeric. Value of the safe planetary boundary.
#' @param highRisk Numeric. Value of the high-risk threshold.
#' @param applyTo Character. Specifies which plots receive the boundary zones.
#'   One of \code{"main"}, \code{"regions"}, or \code{"all"}.
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{\figure{showLinePlotsWithZones.png}{options: width="100\%"}}
#' @examples
#' \dontrun{
#' data <- as.quitte(data)
#' showLinePlotsWithZones(
#'   data,
#'   "Planetary Boundary|Nitrogen|Agricultural Nitrogen surplus",
#'   safePB = 61,
#'   highRisk = 84
# )
#' }
#' @export
showLinePlotsWithZones <- function(...,
                                   safePB,
                                   highRisk,
                                   applyTo = c("main", "all", "regions")) {

  applyTo <- match.arg(applyTo)

  items <- createLinePlots(...)
  if (is.null(items) || length(items) == 0) return(invisible(NULL))

  p1 <- items[[1]]
  p2 <- items[[2]]

  # Build planetary boundary zones,
  if (safePB < highRisk) {
    # Zone order: low risk (green), intermediate (yellow), high risk (red)
    zones <- data.frame(
      ymin = c(-Inf, safePB, highRisk),
      ymax = c(safePB, highRisk, Inf),
      fill = c("#c7e9c0", "#fff7bc", "#fee0d2")
    )
  } else {
    # Zone order: high risk (red), intermediate (yellow), low risk (green)
    zones <- data.frame(
      ymin = c(-Inf, highRisk, safePB),
      ymax = c(highRisk, safePB, Inf),
      fill = c("#fee0d2", "#fff7bc", "#c7e9c0")
    )
  }

  pbLayers <- list(
    ggplot2::geom_rect(
      data = zones,
      ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
      inherit.aes = FALSE,
      alpha = 0.5
    ),
    ggplot2::scale_fill_identity(),
    ggplot2::geom_hline(yintercept = safePB, linetype = 4, linewidth = 1, color = "#33a02c"),
    ggplot2::geom_hline(yintercept = highRisk, linetype = 2, linewidth = 1, color = "#e31a1c")
  )

  if (applyTo %in% c("all", "main")) {
    p1 <- p1 + pbLayers
  }
  if (applyTo %in% c("all", "regions")) {
    p2 <- p2 + pbLayers
  }

  items[[1]] <- p1
  items[[2]] <- p2

  showPlot(layoutLinePlots(items))
  cat("\n\n")
  return(invisible(NULL))
}