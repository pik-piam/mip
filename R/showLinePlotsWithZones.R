

#' @inheritDotParams createLinePlots
#' @return \code{NULL} is returned invisible.
#' @export
showLinePlotsWithZones <- function(...,
                                    safePB,
                                    highRisk,
                                    apply.to = c("main", "all", "regions")) {

  apply.to <- match.arg(apply.to)

  items <- createLinePlots(...)
  if (is.null(items) || length(items) == 0) return(invisible(NULL))

  p1 <- items[[1]]
  p2 <- items[[2]]
  lgnd <- items[[3]]

  # Build planetary boundary zones (green/yellow/red),
  if (safePB < highRisk) {
    # green (low) -> yellow -> red (high)
    zones <- data.frame(
      ymin = c(-Inf, safePB, highRisk),
      ymax = c(safePB, highRisk, Inf),
      fill = c("#c7e9c0", "#fff7bc", "#fee0d2")
    )
  } else {
    # green (high) -> yellow -> red (low)
    zones <- data.frame(
      ymin = c(-Inf, highRisk, safePB),
      ymax = c(highRisk, safePB, Inf),
      fill = c("#fee0d2", "#fff7bc", "#c7e9c0")
    )
  }

  pb_layers <- list(
    ggplot2::geom_rect(
      data = zones,
      ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
      inherit.aes = FALSE,
      alpha = 0.5
    ),
    ggplot2::scale_fill_identity(),
    ggplot2::geom_hline(yintercept = safePB, linetype = 4, linewidth = 1, color = "#33a02c"),
    ggplot2::geom_hline(yintercept = highRisk, linetype = 2, linewidth = 1, color = "#e31a1c")
  )

  if (apply.to %in% c("all", "main")) {
    p1 <- p1 + pb_layers
  }
  if (apply.to %in% c("all", "regions")) {
    p2 <- p2 + pb_layers
  }

  items[[1]] <- p1
  items[[2]] <- p2

  showPlot(layoutLinePlots(items))
  cat("\n\n")
  return(invisible(NULL))
}