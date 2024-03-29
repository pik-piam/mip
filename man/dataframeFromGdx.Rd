% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframeFromGdx.R
\name{dataframeFromGdx}
\alias{dataframeFromGdx}
\title{dataframeFromGdx}
\usage{
dataframeFromGdx(symbolName, pathToGdx, ...)
}
\arguments{
\item{symbolName}{The name of a symbol to be extracted from gdx.}

\item{pathToGdx}{Path to a gdx file.}

\item{...}{Additional arguments passed to gdxrrw::rgdx.}
}
\value{
A data frame with data about symbolName from the given gdx file. All columns are character vectors,
        except the last column which holds numeric values, because that is the format that rgdx returns.
}
\description{
Read data for a given symbol from a gdx file and return it as a dataframe.
}
\details{
This function is similar to gdxrrw::rgdx.param, but it also works for variables.
}
\seealso{
\code{\link{getPlotData}}
}
\author{
Pascal Führlich
}
