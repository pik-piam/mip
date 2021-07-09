#' sideBySidePlots
#'
#' Show multiple plots side by side in one row, using the same plotly slider (frame aesthetic) if that is used.
#'
#' @param ggplots A list of ggplots. These plots are converted via plotly::ggplotly, however, passing plotly plots is
#'                not allowed, because they cannot be customized anymore. When using mipIterations set the argument
#'                returnGgplots to TRUE ot get ggplots instead of plotly plots.
#' @param margin  Margin between plots, passed on to plotly::subplot.
#' @return A tagList containing an h3 for the titles and the ggplots in a plotly::subplot.
#' @author Pascal Führlich
#' @seealso \code{\link{mipIterations}}
#' @importFrom ggplot2 theme element_text ggtitle
#' @importFrom plotly ggplotly add_annotations subplot layout
#' @importFrom htmltools br tagList h4
#' @export
sideBySidePlots <- function(ggplots, margin = 0.05) {
  if (identical(length(ggplots), 0L)) {
    return(taglist())
  }
  # check that elements in ggplots argument have classes gg and ggplot
  stopifnot(all(vapply(ggplots, function(aGgplot) identical(attr(aGgplot, "class"), c("gg", "ggplot")), logical(1))))

  titles <- vapply(ggplots, function(plot) plot[["labels"]][["title"]], character(1))
  # collect titles in a list, separated by 2 html line breaks (br)
  titles <- Reduce(
    function(combinedTitles, index) {
      return(c(combinedTitles, list(br(), br()), paste0(LETTERS[index], ") ", titles[index])))
    },
    seq.int(from = 2, along.with = titles[-1]),
    paste("A)", titles[1])
  )

  plotlyPlots <- lapply(seq_along(ggplots), function(index) {
    aGgplot <- ggplots[[index]] +
      theme(axis.text.x = element_text(angle = 60)) +
      ggtitle("")
    # annotate each plot with the corresponding letter
    plotlyPlot <- ggplotly(aGgplot)
    plotlyPlot <- add_annotations(
      plotlyPlot,
      text = LETTERS[index],
      x = -0.05,
      y = 1.05,
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    )
    plotlyPlot <- layout(plotlyPlot,
                         xaxis = list(title = aGgplot[["labels"]][["x"]]),
                         yaxis = list(title = aGgplot[["labels"]][["y"]]))
    return(plotlyPlot)
  })
  return(tagList(h4(titles), subplot(plotlyPlots, margin = margin, titleX = TRUE, titleY = TRUE)))
}
