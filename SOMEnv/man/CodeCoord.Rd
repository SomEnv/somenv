% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CodeCoord.R
\name{CodeCoord}
\alias{CodeCoord}
\title{Prototype coordinates for graph}
\usage{
CodeCoord(Row, Col)
}
\arguments{
\item{Row}{Number of SOM map rows}

\item{Col}{Number of SOM map columns}
}
\value{
This function returns a \code{data.frame} including columns: 
\itemize{
 \item X
 \item Y
}
}
\description{
Generate X and Y coordinates for plotting a SOM map "Vesanto like"
}
\examples{
Coord<-CodeCoord(10,5)
}
\author{
S. Licen
}