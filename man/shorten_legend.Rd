% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shorten_legend.R
\name{shorten_legend}
\alias{shorten_legend}
\title{Shorten legend names to a given length}
\usage{
shorten_legend(
  x,
  maxchar = 20,
  identical_only = FALSE,
  ylab = NULL,
  sep = c(" ", "|", "_"),
  unit = NULL
)
}
\arguments{
\item{x}{A character vector or a factor vector that should be shortened}

\item{maxchar}{Maximum number of characters allowed}

\item{identical_only}{If set to TRUE identical parts in the name will be removed,
independent of the character length (maxchar will be ignored!)}

\item{ylab}{If specified this part will be removed, independent of maxchar and identical_only}

\item{sep}{A vector of characters which should be interpreted as separators}

\item{unit}{A vector of characters with units, pasted to ylab}
}
\description{
1. If ylab is specified, the function just returns everything after the ylab.
2. If identical_only = TRUE is specified, it removes identical parts in the name, independent of maxchar.
3. If maxchar is specified, the function will first try to return vector as it is, then remove elements
which are identical between all elements and finally cut the end of the character vectors so that it
fits the given maxchar setting. Underscores will be replaced with empty spaces for further processing
}
\examples{
a <- c("Model Scenario_BLUB", "Model Scenario_BLA", "Model Scenario_BLA_BLA_BLUB")

# do nothing
shorten_legend(a, 30)
# remove identical parts
shorten_legend(a, 15)
# or ...
shorten_legend(a, identical_only=TRUE)
# cutoff end of string
shorten_legend(a, 5)
# cut off the first part as explicitly specified
shorten_legend(a, ylab = "Model Scenario")
}
\author{
Jan Philipp Dietrich, Oliver Richters
}
