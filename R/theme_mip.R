#' MIP theme settings
#'
#' @param size Font size
#' @author Jan Philipp Dietrich
#' @examples
#'
#' \dontrun{
#'   p <- mipArea(x) + theme_mip(10)
#' }
#' @importFrom ggplot2 theme element_text unit
#' @export


theme_mip <- function(size=12) {
      return(theme(plot.title      = element_text(size=size+4, face="bold", vjust=1.5),
                   strip.text.x    = element_text(size=size, margin=margin(4,2,4,2,"pt")),
                   axis.title.y    = element_text(angle=90, size=size, face="bold", vjust=1.3),
                   axis.text.y     = element_text(size=size, colour="black"),
                   axis.title.x    = element_text(size=size, face="bold", vjust=-0.3),
                   axis.text.x     = element_text(size=size, angle=90, hjust=.5, colour="black"),
                   legend.text     = element_text(size=size-3),
                   legend.position = "bottom"))
}
